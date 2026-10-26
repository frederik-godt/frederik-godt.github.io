# Læser references.bib og skriver publikationslister som markdown,
# filtreret på nøgleord (keywords) i .bib-posterne.
# Kun base R – ingen ekstra pakker.

read_bib <- function(path) {
  txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  chars <- strsplit(txt, "")[[1]]
  n <- length(chars)
  entries <- list()
  i <- 1
  while (i <= n) {
    if (chars[i] != "@") { i <- i + 1; next }
    j <- i + 1
    while (j <= n && chars[j] != "{") j <- j + 1
    type <- tolower(trimws(paste(chars[(i + 1):(j - 1)], collapse = "")))
    # find matching closing brace for the entry
    depth <- 1; k <- j + 1
    while (k <= n && depth > 0) {
      if (chars[k] == "{") depth <- depth + 1
      if (chars[k] == "}") depth <- depth - 1
      k <- k + 1
    }
    body <- paste(chars[(j + 1):(k - 2)], collapse = "")
    entries[[length(entries) + 1]] <- parse_entry(type, body)
    i <- k
  }
  entries
}

parse_entry <- function(type, body) {
  chars <- strsplit(body, "")[[1]]
  n <- length(chars)
  # citation key = up to first comma
  c1 <- which(chars == ",")[1]
  key <- trimws(paste(chars[1:(c1 - 1)], collapse = ""))
  fields <- list(.type = type, .key = key)
  i <- c1 + 1
  while (i <= n) {
    # field name
    while (i <= n && grepl("[[:space:],]", chars[i])) i <- i + 1
    if (i > n) break
    s <- i
    while (i <= n && chars[i] != "=") i <- i + 1
    if (i > n) break
    name <- tolower(trimws(paste(chars[s:(i - 1)], collapse = "")))
    i <- i + 1
    while (i <= n && grepl("[[:space:]]", chars[i])) i <- i + 1
    if (chars[i] == "{") {
      depth <- 1; i <- i + 1; s <- i
      while (i <= n && depth > 0) {
        if (chars[i] == "{") depth <- depth + 1
        if (chars[i] == "}") depth <- depth - 1
        i <- i + 1
      }
      val <- paste(chars[s:(i - 2)], collapse = "")
    } else if (chars[i] == "\"") {
      i <- i + 1; s <- i
      while (i <= n && chars[i] != "\"") i <- i + 1
      val <- paste(chars[s:(i - 1)], collapse = ""); i <- i + 1
    } else {
      s <- i
      while (i <= n && chars[i] != ",") i <- i + 1
      val <- trimws(paste(chars[s:(i - 1)], collapse = ""))
    }
    fields[[name]] <- val
  }
  fields
}

clean_tex <- function(x) {
  # \u-koder gør filen uafhængig af systemets tegnsæt (virker også på Windows)
  from <- c("{\\aa}", "{\\AA}", "{\\o}", "{\\O}", "{\\ae}", "{\\AE}",
            "\\aa ", "\\o ", "\\ae ", "--")
  to   <- c("\u00e5", "\u00c5", "\u00f8", "\u00d8", "\u00e6", "\u00c6",
            "\u00e5", "\u00f8", "\u00e6", "\u2013")
  for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE)
  x <- gsub("{", "", x, fixed = TRUE)
  gsub("}", "", x, fixed = TRUE)
}

format_authors <- function(a) {
  if (is.null(a)) return("")
  a <- clean_tex(a)
  people <- trimws(strsplit(a, "\\s+and\\s+")[[1]])
  out <- vapply(people, function(p) {
    parts <- trimws(strsplit(p, ",")[[1]])
    if (length(parts) < 2) return(p)
    given <- strsplit(parts[2], "[ -]+")[[1]]
    inits <- paste0(substr(given, 1, 1), ".", collapse = " ")
    paste0(parts[1], ", ", inits)
  }, character(1))
  if (length(out) == 1) out
  else paste0(paste(out[-length(out)], collapse = ", "), " & ", out[length(out)])
}

# Links der vises efter hver reference. Rækkefølgen her er rækkefølgen på siden.
# Venstre side = feltnavn i .bib-filen, højre side = tekst på linket.
# Ret teksten frit, fx "Journal version" -> "DOI".
LINK_LABELS <- c(
  doi      = "Journal version",
  pdf      = "PDF",
  oa       = "Open access",
  preprint = "Preprint",
  url      = "Link"
)

make_link <- function(field, value) {
  value <- trimws(clean_tex(value))
  if (field == "doi") value <- paste0("https://doi.org/", sub("^https?://(dx\\.)?doi\\.org/", "", value))
  paste0("[", LINK_LABELS[[field]], "](", value, "){.pub-link}")
}

format_entry <- function(e) {
  g <- function(f) if (is.null(e[[f]])) NULL else clean_tex(e[[f]])
  s <- paste0(format_authors(e$author), " (", g("year"), "). ", g("title"), ".")
  if (!is.null(g("journal"))) {
    s <- paste0(s, " *", g("journal"), "*")
    if (!is.null(g("volume"))) s <- paste0(s, ", ", g("volume"))
    if (!is.null(g("number"))) s <- paste0(s, "(", g("number"), ")")
    if (!is.null(g("pages"))) s <- paste0(s, ", ", g("pages"))
    s <- paste0(s, ".")
  }
  if (!is.null(g("note"))) s <- paste0(s, " ", g("note"), ".")
  links <- character(0)
  for (f in names(LINK_LABELS)) {
    if (!is.null(e[[f]]) && nzchar(trimws(e[[f]]))) links <- c(links, make_link(f, e[[f]]))
  }
  if (length(links) > 0) s <- paste0(s, " ", paste(links, collapse = " "))
  s
}

# Abstract / kort beskrivelse (feltet `abstract` i .bib-filen).
# ABSTRACT_MODE: "collapsible" = foldes ud ved klik, "visible" = altid synlig
ABSTRACT_MODE  <- "visible"

format_abstract <- function(e) {
  if (is.null(e$abstract) || !nzchar(trimws(e$abstract))) return(NULL)
  txt <- trimws(gsub("[[:space:]]+", " ", clean_tex(e$abstract)))
  txt <- gsub("&", "&amp;", txt, fixed = TRUE)   # abstract skrives som ren tekst
  txt <- gsub("<", "&lt;", txt, fixed = TRUE)
  txt <- gsub(">", "&gt;", txt, fixed = TRUE)
  # Pandoc-"raw html"-blok: sendes uændret igennem, så strukturen ikke ændres
  html <- if (ABSTRACT_MODE == "visible") {
    paste0("<div class=\"pub-abstract\"><p>", txt, "</p></div>")
  } 
}

has_kw <- function(e, kw) {
  if (is.null(kw)) return(TRUE)
  k <- trimws(strsplit(e$keywords %||% "", ",")[[1]])
  kw %in% k
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Brug i et chunk med  results: asis
pub_list <- function(bib_path, theme = NULL, status = NULL, empty = "*Ingen endnu.*") {
  bib <- read_bib(bib_path)
  keep <- Filter(function(e) has_kw(e, theme) && has_kw(e, status), bib)
  if (length(keep) == 0) { cat(empty, "\n\n"); return(invisible()) }
  yrs <- vapply(keep, function(e) suppressWarnings(as.numeric(e$year %||% 0)), numeric(1))
  keep <- keep[order(-yrs)]
  for (e in keep) {
    cat("- ", format_entry(e), "\n", sep = "")
    ab <- format_abstract(e)
    if (!is.null(ab)) cat("\n", ab, sep = "")
  }
  cat("\n")
}