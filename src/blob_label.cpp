// 3D connected-component labelling for heatwave-event masks.
// Two-pass scan with union-find. 6-connectivity (face-adjacent).
// Optional dateline wrap on the x-axis.
//
// Mask layout matches R's column-major 3D array:
//   m[i, j, k] -> mask[i + nx*j + nx*ny*k]
// with i in [0, nx), j in [0, ny), k in [0, nt).
//
// Returned labels are dense, 1..K. Background voxels carry 0.

#include <Rcpp.h>
#include <vector>
#include <cstdint>

using namespace Rcpp;

static inline int uf_find(std::vector<int>& parent, int x) {
  while (parent[x] != x) {
    parent[x] = parent[parent[x]]; // path halving
    x = parent[x];
  }
  return x;
}

static inline void uf_union(std::vector<int>& parent,
                            std::vector<int>& rank_,
                            int a, int b) {
  int ra = uf_find(parent, a);
  int rb = uf_find(parent, b);
  if (ra == rb) return;
  if (rank_[ra] < rank_[rb]) {
    parent[ra] = rb;
  } else if (rank_[ra] > rank_[rb]) {
    parent[rb] = ra;
  } else {
    parent[rb] = ra;
    rank_[ra]++;
  }
}

// [[Rcpp::export]]
IntegerVector label_components_3d_cpp(IntegerVector mask,
                                      int nx, int ny, int nt,
                                      bool wrap_dateline = false) {
  const R_xlen_t n = static_cast<R_xlen_t>(nx) * ny * nt;
  if (mask.size() != n) {
    stop("mask length does not match nx * ny * nt");
  }

  IntegerVector labels(n);  // initialised to 0

  // Union-find. Reserve to amortise growth.
  std::vector<int> parent;
  std::vector<int> rank_;
  parent.reserve(1024);
  rank_.reserve(1024);
  // sentinel for index 0 (unused; labels start at 1)
  parent.push_back(0);
  rank_.push_back(0);

  int next_label = 1;

  // Pass 1: raster scan in (i, j, k) order, with column-major indexing.
  const R_xlen_t stride_y = nx;
  const R_xlen_t stride_t = static_cast<R_xlen_t>(nx) * ny;

  for (int k = 0; k < nt; ++k) {
    const R_xlen_t base_t = static_cast<R_xlen_t>(k) * stride_t;
    for (int j = 0; j < ny; ++j) {
      const R_xlen_t base_yt = base_t + static_cast<R_xlen_t>(j) * stride_y;
      for (int i = 0; i < nx; ++i) {
        const R_xlen_t v = base_yt + i;
        if (mask[v] == 0) continue;

        int lx = (i > 0) ? labels[v - 1] : 0;
        int ly = (j > 0) ? labels[v - stride_y] : 0;
        int lt = (k > 0) ? labels[v - stride_t] : 0;

        int neigh[3] = {lx, ly, lt};
        int chosen = 0;
        for (int s = 0; s < 3; ++s) {
          if (neigh[s] > 0) {
            if (chosen == 0 || neigh[s] < chosen) chosen = neigh[s];
          }
        }

        if (chosen == 0) {
          // start a new component
          parent.push_back(next_label);
          rank_.push_back(0);
          labels[v] = next_label;
          ++next_label;
        } else {
          labels[v] = chosen;
          for (int s = 0; s < 3; ++s) {
            if (neigh[s] > 0 && neigh[s] != chosen) {
              uf_union(parent, rank_, chosen, neigh[s]);
            }
          }
        }
      }
    }
  }

  // Dateline wrap: union edges across the x boundary, then re-resolve.
  if (wrap_dateline && nx >= 2) {
    for (int k = 0; k < nt; ++k) {
      const R_xlen_t base_t = static_cast<R_xlen_t>(k) * stride_t;
      for (int j = 0; j < ny; ++j) {
        const R_xlen_t base_yt = base_t + static_cast<R_xlen_t>(j) * stride_y;
        const R_xlen_t v0 = base_yt;
        const R_xlen_t v1 = base_yt + (nx - 1);
        if (labels[v0] > 0 && labels[v1] > 0) {
          uf_union(parent, rank_, labels[v0], labels[v1]);
        }
      }
    }
  }

  // Pass 2: resolve labels via union-find and compactify to 1..K.
  // First, find the canonical root for each label; build a dense remap.
  std::vector<int> remap(parent.size(), 0);
  int next_dense = 1;
  for (R_xlen_t v = 0; v < n; ++v) {
    int lab = labels[v];
    if (lab == 0) continue;
    int root = uf_find(parent, lab);
    int dense = remap[root];
    if (dense == 0) {
      dense = next_dense++;
      remap[root] = dense;
    }
    labels[v] = dense;
  }

  labels.attr("dim") = IntegerVector::create(nx, ny, nt);
  labels.attr("n_components") = next_dense - 1;
  return labels;
}
