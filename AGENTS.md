# Agent Instructions

## Theme Contract

This fork keeps the spirit of `rougier/nano-theme`: a usable Emacs theme should
derive mode-specific styling from the default face plus the six semantic Nano
faces:

- `nano-critical`: immediate action or hard failure
- `nano-popout`: attention without structural importance
- `nano-strong`: structure and titles
- `nano-salient`: important links, matches, and positive state
- `nano-faded`: secondary information
- `nano-subtle`: quiet screen regions

## Editing Rules

- Prefer semantic inheritance or `nano-face-merge` over introducing new colors.
- Add direct colors only for terminal palettes or other domains where the
  external protocol already defines color slots.
- Keep support static and cheap: extend the existing `custom-theme-set-faces`
  form instead of adding per-mode runtime hooks.
- New mode support should be grouped by package family and should explain any
  non-obvious palette choice in a short comment.
- Do not vendor broad upstream rewrites unless Hyunggyu explicitly asks for a
  full upstream sync. Port the useful face coverage while preserving this
  fork's optimized single-file shape.

## Verification

Run at least:

```sh
emacs --batch -L . -l nano-theme.el --eval '(load-theme '"'"'nano t)'
```

When changing package-specific faces, also load the package if available and
confirm the relevant face symbols resolve.
