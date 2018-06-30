sorted.importance.dataframe <- function(res.forest){
    imp.data <- data.frame(
        name = (attributes(res.forest$importance)$dimnames)[[1]], 
        importance = c(res.forest$importance))
    imp.data$name <- factor(imp.data$name, levels = imp.data$name[order(imp.data$importance)])
    return(imp.data)
}

important.bar.plot <- function(res.forest, ...){
    ggplot(sorted.importance.dataframe(res.forest), aes(x = name, y = importance)) +
        geom_bar(stat="identity", fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 75, hjust = 1), ...)
}

important.dot.plot <- function(res.forest, ...){
    ggplot(sorted.importance.dataframe(res.forest), aes(x = name, y = importance)) + 
        geom_point(col = "steelblue") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 75, hjust = 1), ...) +
        geom_line(aes(x = name, y = importance, group=1), alpha = 0.4)
}
