---
name: heatwaveR
description: Documentation site for heatwaveR, a research-grade R package for detecting and classifying marine heatwaves and cold-spells.
colors:
  deep-sea-teal: "#0B7285"
  slate-current: "#546F78"
  kelp-green: "#2C7A5B"
  shallow-water-blue: "#2B8CBE"
  tide-amber: "#D97706"
  deep-coral: "#A61E4D"
  sea-foam: "#F4FBFC"
  benthic-ink: "#14303A"
  body-text-light: "#2A444D"
  sea-mist: "#CFE0E4"
  abyss-navy: "#102733"
  abyss-deep: "#0d1c24"
  abyss-depth: "#11212a"
  shallow-reef: "#D9EFF2"
  foam-glass: "rgba(255, 255, 255, 0.84)"
  abyss-glass: "rgba(16, 39, 51, 0.86)"
  current-glow: "rgba(66, 192, 212, 0.14)"
  reef-glow: "rgba(132, 209, 221, 0.14)"
typography:
  display:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "clamp(2.4rem, 5vw, 4rem)"
    fontWeight: 500
    lineHeight: 0.95
    letterSpacing: "-0.05em"
  headline:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "2rem"
    fontWeight: 500
    lineHeight: 1.2
    letterSpacing: "-0.03em"
  title:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "1.05rem"
    fontWeight: 500
    lineHeight: 1.3
  body:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "1rem"
    fontWeight: 400
    lineHeight: 1.5
  lead:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "1.15rem"
    fontWeight: 400
    lineHeight: 1.65
  label:
    fontFamily: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', 'Noto Sans', Arial, sans-serif"
    fontSize: "1rem"
    fontWeight: 600
    letterSpacing: "-0.02em"
rounded:
  sm: "0.375rem"
  md: "1rem"
  lg: "1.25rem"
  xl: "1.5rem"
  pill: "999px"
spacing:
  xs: "0.5rem"
  sm: "0.75rem"
  md: "1rem"
  lg: "1.25rem"
  xl: "1.75rem"
  xxl: "2.1rem"
components:
  badge-pill:
    backgroundColor: "{colors.foam-glass}"
    rounded: "{rounded.pill}"
    padding: "0.1rem"
  card:
    backgroundColor: "{colors.foam-glass}"
    rounded: "{rounded.lg}"
    padding: "1.25rem"
  nav-link:
    textColor: "{colors.body-text-light}"
    typography: "{typography.label}"
  navbar-brand:
    textColor: "{colors.body-text-light}"
    typography: "{typography.label}"
  search-input:
    backgroundColor: "#FFFFFF"
    textColor: "{colors.body-text-light}"
    rounded: "{rounded.sm}"
    padding: "0.375rem 0.75rem"
---

# Design System: heatwaveR

## Overview

**Creative North Star: "Calm Water, Warming Signal"**

The site is a cool, quiet instrument that gets out of the way of the science: a teal-and-navy "ocean" surface — soft gradients, translucent glass cards, gentle shadows — that reads as calm, precise water. Against that calm sits the one deliberate moment of heat: the package's flame-chart imagery and hex-sticker mark, rendered in a warm amber-to-maroon gradient. The whole system is built on this single contrast — cool water holding a warming signal — rather than on decoration for its own sake.

The mood is calm, precise, and quietly confident: no startup gloss, no playful/rounded SaaS energy, no dense dashboard chrome. It should read as a serious research tool that happens to be well made — components look like trustworthy research infrastructure (a "quietly credible instrument panel"), not marketing widgets. Depth is atmospheric rather than skeuomorphic: surfaces are soft-lifted and translucent, floating a little above a quiet gradient wash rather than stacking like physical material.

**Key Characteristics:**
- A cool teal/navy "water" palette carries almost the entire interface; warm color is reserved for the one signal that matters (the flame/heatwave imagery).
- Soft-lifted, translucent "glass" surfaces (cards, badges, navbar) over a faint radial/linear gradient body wash — never flat white, never hard drop shadows.
- Tight, confident display type (negative letter-spacing, clamped fluid sizing) reserved for the homepage title; everything else is restrained system-UI type.
- Generously rounded, pill-and-soft-rectangle geometry (6px–24px, plus true pills for badges) — no sharp corners anywhere in the custom layer.
- A full light/dark theme pair, both true to the same water metaphor (pale ice-teal by day, deep abyss-navy by night).

## Colors

A narrow, cool "ocean" palette (teal primary, slate-blue secondary, ice-teal and abyss-navy neutrals) that stays out of the way, plus a standard Bootstrap semantic set for state. Warm color is deliberately absent from this system — it is reserved entirely for the package's flame/category plot output (see **Components → Severity Scale** below), never for site chrome.

### Primary
- **Deep Sea Teal** (`#0B7285`): the site's one accent — links, active nav state, the primary brand color referenced by the logo mark's icon and by `--bs-primary`. Used sparingly against the neutral water tones.

### Secondary
- **Slate Current** (`#546F78`): muted secondary text and quieter UI accents (`--bs-secondary`); a desaturated cousin of the primary that never competes with it. Darkened from an earlier `#5F7E88` after an audit found the lighter value failed WCAG AA contrast (4.15:1) against Sea Foam in the sitewide navbar version label — the corrected value holds the same hue/saturation at 5.11:1.

### Semantic
- **Kelp Green** (`#2C7A5B`) — success state (`--bs-success`).
- **Shallow Water Blue** (`#2B8CBE`) — informational state (`--bs-info`).
- **Tide Amber** (`#D97706`) — warning state (`--bs-warning`). Distinct from, and not to be confused with, the flame chart's amber category color — this is UI chrome, that is fixed scientific convention.
- **Deep Coral** (`#A61E4D`) — danger/error state (`--bs-danger`).

### Neutral
- **Sea Foam** (`#F4FBFC`): the light-mode page background — a near-white pale ice-teal, never pure white.
- **Benthic Ink** (`#14303A`): the authored foreground value (`--bs-black`) — used where a true near-black is needed (e.g. inherited heading color). It is *not* the color that paints running paragraph text; see Body Text Light below.
- **Body Text Light** (`#2A444D`): the color that actually renders light-mode body copy, via the theming engine's `--bs-body-color`/`--bs-gray-900` chain derived from Benthic Ink. Slightly lighter and more teal than the authored `#14303A` — both are legitimate, but this is the one to check when validating text contrast.
- **Sea Mist** (`#CFE0E4`): light-mode borders and dividers — a soft, barely-there teal-grey.
- **Abyss Navy** (`#102733`): the dark-mode surface color — page background (mid-gradient), navbar, and card fills at night.
- **Abyss Deep** (`#0d1c24`): the darkest stop in the dark-mode body background gradient (top of page); hand-authored in `extra.scss`, not theme-derived.
- **Abyss Depth** (`#11212a`): the dark-mode body gradient's lower stop, a hair darker than Abyss Navy; also hand-authored.
- **Shallow Reef** (`#D9EFF2`): a soft accent tint for gentle highlighted surfaces (kept in reserve; not yet wired into a live light-mode component, but the system's designated "pale accent" step).
- **Foam Glass** (`rgba(255, 255, 255, 0.84)`): the translucent light-mode "glass" surface used for cards, badges, and the hero block.
- **Abyss Glass** (`rgba(16, 39, 51, 0.86)`): the translucent dark-mode counterpart to Foam Glass, used on dark-mode cards, badges, and tables.
- **Current Glow** (`rgba(66, 192, 212, 0.14)`): a hand-authored dark-mode ambient cyan glow — the radial highlight behind the dark-mode body and the wash behind the dark-mode hero block (also appears at a fainter `0.08` opacity in the hero's second gradient layer).
- **Reef Glow** (`rgba(132, 209, 221, 0.14)`): a hand-authored pale-aqua hairline border/glow tint used on the dark-mode hero and card borders (also appears at `0.12` opacity on cards and at a related `rgba(217, 239, 242, 0.08)` on the dark navbar's bottom border).

Dark mode is a full companion theme, not a single inverted filter — and it is a mix of two sources, not one. Body text, standard borders, and link color shift via the theming engine's automatic dark-mode contrast tuning (soft sea-fog grey text, mid slate-depth borders, a brighter aqua link). But the atmospheric layer — the body/hero background gradients and the glow/border accents on the hero and cards (Abyss Deep, Abyss Depth, Current Glow, Reef Glow) — is hand-authored directly in `extra.scss`, not theme-derived, and must be preserved deliberately rather than assumed to regenerate on its own.

### Named Rules
**The One Warm Voice Rule.** Warm hues (amber, orange, red, maroon) never appear in site chrome, navigation, or UI state. They belong exclusively to the package's flame/category data visualization, so that when a warm color does appear, it always means "this is the science, this is the heat."

## Typography

**Display Font:** system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", "Noto Sans", Arial, sans-serif (the OS-native sans stack; no custom webfont is loaded)
**Body Font:** the same system-ui stack throughout — one typeface family for the whole site.

**Character:** A single, unadorned system sans is used everywhere; personality comes entirely from scale, weight, and spacing rather than typeface choice. The homepage title is the one place type gets expressive — tightly tracked and fluidly scaled — everything else stays quiet.

### Hierarchy
- **Display** (500, `clamp(2.4rem, 5vw, 4rem)`, line-height 0.95, letter-spacing -0.05em): the homepage `<h1>` only — tight, confident, fluidly sized between mobile and desktop.
- **Headline** (500, ~2rem, letter-spacing -0.03em): homepage section headers ("Install", "Start here", "Where to go next").
- **Title** (500, 1.05rem): card headings inside the "Start here" grid.
- **Body** (400, 1rem, line-height 1.5): default running text and documentation prose.
- **Lead** (400, 1.15rem, line-height 1.65, max-width 60ch): a wider variant of Body used for the homepage's opening paragraphs.
- **Label** (600–700, letter-spacing -0.02em on the wordmark): navbar brand and nav links; the wordmark is the heavier of the two (700).

### Named Rules
**The Negative-Tracking Display Rule.** Anything set at display scale (the homepage `<h1>`) tightens letter-spacing to -0.05em and line-height to 0.95 — confidence reads through compression, not size alone.

## Layout

The site runs on a standard Bootstrap 5 container/grid, with one custom layout primitive layered on top: a CSS grid of cards (`repeat(auto-fit, minmax(215px, 1fr))`, 1rem gap) that reflows from a single column on mobile to as many columns as fit at ≥215px each. The homepage's intro/hero content composes with a two-column grid at ≥992px (`minmax(0, 1.9fr) minmax(180px, 0.8fr)`, 2rem–2.1rem padding) that collapses to a single stacked column below that breakpoint. The documentation sidebar is sticky (`top: 70px`) on desktop reference/article pages so navigation stays in view while scrolling long content. Rhythm is generous and consistent: section spacing lives in the 1.5rem–2.75rem range; component-internal padding sits at 1.25rem–2.1rem.

## Elevation & Depth

Depth is soft-lifted and translucent, not skeuomorphic or hard-edged: surfaces are semi-transparent "glass" fills (Foam Glass / Abyss Glass) floating over a faint radial-plus-linear gradient body wash, held up by diffuse, low-contrast shadows rather than crisp drop shadows. Nothing sits flush against a hard white or black background — every surface reads as suspended slightly above the water beneath it.

### Shadow Vocabulary
- **Navbar shadow** (`0 10px 30px rgba(16, 39, 51, 0.08)`): a wide, very soft shadow under the sticky top navigation.
- **Hero/card lift** (`0 18px 36px rgba(16, 39, 51, 0.09)` for the hero block, `0 16px 32px rgba(16, 39, 51, 0.07)` for grid cards): the system's primary "floating surface" shadow — wide spread, low opacity.
- **Badge lift** (`0 8px 18px rgba(16, 39, 51, 0.06)`): a lighter version of the same shadow for small pill chips.
- **Code/table lift** (`0 14px 30px rgba(16, 39, 51, 0.08)` light / `0 18px 34px rgba(0, 0, 0, 0.24)` dark): applied to code blocks and tables so they read as raised reference panels.

### Named Rules
**The No-Hard-Edge Rule.** Every custom surface (hero, card, badge, table, code block) gets a soft, wide-spread shadow and a rounded corner. Nothing in the custom layer is allowed to sit flat with a hard edge against the page.

## Shapes

Corners are generously rounded throughout the custom layer, scaling with a surface's size: 6px (`.375rem`, unmodified Bootstrap default) on small default controls like buttons and inputs, 16px (`1rem`) on sidebars, tables, and code blocks, 20px (`1.25rem`) on grid cards, 24px (`1.5rem`) on the largest hero surface, and a true pill (`999px`) on badge chips. Borders are hairline (1px) and low-contrast — either the neutral Sea Mist border or a primary-tinted `rgba(11, 114, 133, 0.14)` — never a heavy or high-contrast rule.

## Components

Components are understated and precise — a "quietly credible instrument panel": nothing decorative, every surface earns its shadow and radius by actually floating a piece of content above the water-wash background.

### Cards
- **Corner Style:** 20px (`1.25rem`).
- **Background:** Foam Glass (light) / Abyss Glass (dark).
- **Shadow Strategy:** the hero/card lift shadow (see Elevation & Depth); a hairline primary-tinted border (`rgba(11, 114, 133, 0.14)`) reinforces the edge instead of a hard line.
- **Internal Padding:** 1.25rem.
- **Content shape:** title (Title scale) + short body copy + a single trailing link — never more than one call to action per card.

### Badges (pkgdown status chips)
- **Style:** pill-shaped (`999px`), Foam Glass background, badge-lift shadow, minimal internal padding (`0.1rem`) so the wrapped badge image reads edge-to-edge inside a soft chip.
- **State:** static; badges are informational, not interactive.

### Navigation
- **Style:** a sticky top navbar with a frosted-glass treatment (`backdrop-filter: blur(10px)`), a soft wide shadow, and a hairline bottom border. The brand mark pairs a small square logo icon with a bold (700), tightly-tracked wordmark; nav links are semibold (600) and inherit the body text color rather than using the accent color for their resting state, so the accent stays reserved for the active/hover link and inline text links elsewhere on the page.
- **Mobile:** collapses to a standard Bootstrap toggler/hamburger pattern; no bespoke mobile nav treatment beyond that.

### Inputs (search)
- **Style:** default Bootstrap form-control geometry (6px radius, Sea Mist border, white fill) — the one place the site does not apply its own glass/shadow treatment, keeping the search field visually "native" and unambiguous.
- **Focus:** standard Bootstrap focus ring, tinted with the primary color.

### Severity Scale (signature — fixed, not part of this design system)
The package's plotting functions (`event_line()`, `geom_flame()`) render marine-heatwave severity using a hard-coded four-step category scale that matches the Hobday et al. (2018) convention used across the marine-heatwave research community: **Moderate** `#ffc866`, **Strong** `#ff6900`, **Severe** `#9e0000`, **Extreme** `#2d0000` (a parallel cool-toned scale — `#C7ECF2` → `#85B7CC` → `#4A6A94` → `#111433` — is used for cold-spells). The hex-sticker logo's flame motif is drawn from this same scale. This is documented here for reference only: it is a scientific/cross-tool convention, not a brand choice, and must never be restyled to match the site's teal/navy palette or revised independently of the field's own convention.

## Do's and Don'ts

### Do:
- **Do** keep warm hues (amber/orange/red/maroon) exclusive to the flame/category plot output and the logo — never introduce them into site chrome, nav, or buttons (**The One Warm Voice Rule**).
- **Do** give every custom surface a soft, wide-spread shadow and a rounded corner scaled to its size — 16px for panels/tables, 20px for cards, 24px for the hero, true pills for badges (**The No-Hard-Edge Rule**).
- **Do** use translucent Foam Glass / Abyss Glass fills for floating surfaces rather than opaque white or black.
- **Do** reserve display-scale type (negative tracking, clamped fluid size) for the single homepage `<h1>`; keep section headers and body copy in the quieter Headline/Body roles.
- **Do** treat dark mode as a full companion theme (own background, text, border, and link colors), not an inverted filter.

### Don't:
- **Don't** restyle the MHW/MCS severity color scale (`#ffc866` → `#2d0000`, and the cold-spell equivalent) to match the site palette — it is a fixed cross-tool scientific convention, documented but out of scope for this design system.
- **Don't** use hard, high-contrast borders or crisp drop shadows anywhere in the custom layer — depth here is soft and atmospheric.
- **Don't** introduce a second display typeface or a decorative/script font for site chrome — the system-UI stack is deliberate and uniform. (The hex-sticker logo's script wordmark is a static brand asset, not a live type role.)
- **Don't** style the search input with the glass/shadow treatment used elsewhere — it intentionally stays native Bootstrap so it reads unambiguously as a functional control.
