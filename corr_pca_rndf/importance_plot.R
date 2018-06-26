imp.data <- data.frame(nms = (attributes(res.forest$importance)$dimnames)[[1]], val = c(res.forest$importance))

imp.data <- imp.data[order(imp.data$val, decreasing = T), ]
plot(imp.data$val)
ggplot(imp.data, aes(x = nms, y = val)) +
    geom_bar(stat="identity", fill="steelblue") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
