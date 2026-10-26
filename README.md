# Academic website (Quarto, English only)

1. Open the folder in RStudio (File > New Project > Existing Directory).
2. Run `quarto preview` in the Terminal.
3. Replace all `[...]` fields, `din.mail@sdu.dk`, ORCID, Scholar ID and GitHub username.
4. Add `profile.jpg` (square, ~600x600 px) and `cv.pdf` to the root folder.

Publications: edit `references.bib`. The keywords field controls where an entry appears
(theme: trust / burdens; status: published / workingpaper). See `research.qmd`.

Publish: `quarto publish gh-pages`, or run `quarto render` and drag `docs/` to netlify.com.
