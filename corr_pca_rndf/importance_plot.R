sorted.importance.dataframe <- function(res.forest){
    imp.data <- data.frame(
        nms = (attributes(res.forest$importance)$dimnames)[[1]], 
        val = c(res.forest$importance))
    imp.data$nms <- factor(imp.data$nms, levels = imp.data$nms[order(imp.data$val)])
    return(imp.data)
}

important.bar.plot <- function(res.forest, ...){
    ggplot(sorted.importance.dataframe(res.forest), aes(x = nms, y = val)) +
        geom_bar(stat="identity", fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 75, hjust = 1), ...)
}

important.dot.plot <- function(res.forest, ...){
    ggplot(sorted.importance.dataframe(res.forest), aes(x = nms, y = val)) + 
        geom_point(col = "steelblue") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 75, hjust = 1), ...) +
        geom_line(aes(x = nms, y = val, group=1), alpha = 0.4)
}
