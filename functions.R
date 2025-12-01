
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
    
    # Markdown link if not empty
    if (!is.na(link) && nzchar(link)) {
        title <- paste0("[", title, "](", link, ")")
    }
    
    # Formatting
    text <- paste0(authors, " (", year, ") ", title, " *", journal, "*, ", volume,
                   ". **", Q, ", IF: ", IF, ", IC: ", IC, note_part, "**")
    
    # Check for "important" being "yes" while handling NA
    if (!is.na(important) && important == "yes") {
        text <- paste0("- ", "<span style='color: #00ab44;'>", text, "</span>")
    } else {
        text <- paste0("- ", text)
    }
    
    return(text)
}
