#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// http://systematicinvestor.github.io/Run-Quantile-Rcpp

// [[Rcpp::plugins(cpp11)]]

// quantile setup
struct quantile {
	int lo, hi, n, total;
	double hlo, hhi;
};
quantile make_quantile(int n, double prob) {
	quantile res;
	double index = (n - 1) * prob;
	res.lo = floor(index);
	res.hi = res.lo + 1;
	res.hhi = index - res.lo;
	res.hlo = 1 - res.hhi;
	return res;
}

// index vector
vector<int> make_seq(int n) {
	vector<int> id(n);
	iota(id.begin(), id.end(), 0);
	return id;
}

// [[Rcpp::export]]
NumericVector run_quantile0(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);

	// quantile setup
	auto q = make_quantile(n, prob);

	for(int i = 0; i < (sz-n+1); i++) {
		// can be made a lot faster by not re-sorting each time
		vector<double> z(&x[i], &x[i+n]);
		sort(z.begin(), z.end());
		res[i+n-1] = q.hlo * z[q.lo] + q.hhi * z[q.hi];
	}

	// pad the first n-1 elements with NA
	fill(res.begin(), res.end()-sz+n-1, NA_REAL);
	return res;
}

// [[Rcpp::export]]
NumericVector run_quantile(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);

	// quantile setup
	auto q = make_quantile(n, prob);

	// index vector
	auto id = make_seq(n);

	for(int i = 0; i < (sz-n+1); i++) {
		if(i == 0)
			sort(id.begin(), id.end(),
				[&](int a, int b) { return x[a] < x[b]; });
		else {
	    		// remove index (i-1)
		    	id.erase(find(id.begin(), id.end(), i-1));
		    	// insert keeping sorted order
	    		id.insert(lower_bound(id.begin(), id.end(), i+n-1,
	    			[&](int a, int b) { return x[a] < x[b]; }), i+n-1);
		}

		res[i+n-1] = q.hlo * x[id[q.lo]] + q.hhi * x[id[q.hi]];
	}

	// pad the first n-1 elements with NA
	fill(res.begin(), res.end()-sz+n-1, NA_REAL);
	return res;
}

// [[Rcpp::export]]
NumericVector run_quantile_matrix(NumericVector x, int n, NumericVector probs) {
	auto sz = x.size();
	auto nprobs = probs.size();

	NumericMatrix res(sz, nprobs);

	// quantile setup
	vector<quantile> qs(nprobs);
	for(int j = 0; j < nprobs; j++)
		qs[j] = make_quantile(n, probs[j]);

	// index vector
	auto id = make_seq(n);

	for(int i = 0; i < (sz-n+1); i++) {
		if(i == 0)
			sort(id.begin(), id.end(),
				[&](int a, int b) { return x[a] < x[b]; });
		else {
			// remove index (i-1)
			id.erase(find(id.begin(), id.end(), i-1));
			// insert keeping sorted order
			id.insert(lower_bound(id.begin(), id.end(), i+n-1,
	    			[&](int a, int b) { return x[a] < x[b]; }), i+n-1);
		}

		for(int j = 0; j < nprobs; j++) {
			auto q = qs[j];
        		res(i+n-1, j) = q.hlo * x[id[q.lo]] + q.hhi * x[id[q.hi]];
		}
	}

	// move to Rcpp11 - http://stackoverflow.com/questions/23748572/initializing-a-matrix-to-na-in-rcpp
	// pad the first n-1 elements with NA
	for (int i = 0; i < (n-1); i++)
		for(int j = 0; j < nprobs; j++)
			res(i,j) = NA_REAL;

	return res;
}
