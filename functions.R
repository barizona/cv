
#xxxxxxxxxxxxxxx
# Bold specific authors -------------------------------------------------------
#xxxxxxxxxxxxxxx

authors_to_bold <- c("Ari E")


#xxxxxxxxxxxxxxx
# Preprints ---------------------------------------------------------------
#xxxxxxxxxxxxxxx

# format the table function
format_publication_unpublished <- function(authors, year, title, link, journal, 
                                           volume, note) {
    for (name in authors_to_bold) {
        authors <- gsub(name, paste0("**", name, "**"), authors, fixed = TRUE)
    }
    
    # Add note only if it is not NA
    note_part <- if(!is.na(note)) paste0(" ", note) else ""
    
    # Markdown link if not empty
    if (!is.na(link) && nzchar(link)) {
        title <- paste0("[", title, "](", link, ")")
    }
    
    # Formatting
    paste0("- ", authors, " (", year, ") ", title, " *", journal, "*, ", volume,
           ".", note_part)
}


#xxxxxxxxxxxxxxx
# Published ---------------------------------------------------------------
#xxxxxxxxxxxxxxx

# Format the table function
format_publication_published <- function(authors, year, title, link, journal, 
                                         volume, Q, IF, IC, note, important) {
    
    for (name in authors_to_bold) {
        authors <- gsub(name, paste0("**", name, "**"), authors, fixed = TRUE)
    }
    
    # Add note only if it is not NA
    note_part <- if (!is.na(note)) paste0(" ", note) else ""
    
    # Detect "Book Chapter" in note (robust to NA)
    is_book_chapter <- !is.na(note) && grepl("Book Chapter|Könyvfejezet", note, ignore.case = TRUE)
    
    # Markdown link if not empty
    if (!is.na(link) && nzchar(link)) {
        title <- paste0("[", title, "](", link, ")")
    }
    
    # Build metrics part conditionally:
    # - Book Chapter: show only IC (plus note)
    # - Otherwise: show Q, IF, IC (plus note)
    metrics_part <- if (is_book_chapter) {
        paste0(" **IC: ", IC, note_part, "**")
    } else {
        paste0(" **", Q, ", IF: ", IF, ", IC: ", IC, note_part, "**")
    }
    
    # Formatting
    text <- paste0(authors, " (", year, ") ", title, " *", journal, "*, ", 
                   volume, ".", metrics_part)
    
    # Check for "important" being "yes" while handling NA
    if (!is.na(important) && important == "yes") {
        text <- paste0("- ", "<span style='color: #00ab44;'>", text, "</span>")
    } else {
        text <- paste0("- ", text)
    }
    
    return(text)
}
