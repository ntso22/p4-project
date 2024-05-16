library("ggplot2")
library("readr")
library("reshape2")

RETURNS <- data.frame(read_csv("prices.csv"))

cor_matrix <- cor(RETURNS)

cor_df <- rev(data.frame(id=colnames(cor_matrix), cor_matrix))

cor_df$IBM

gg <- melt(cor_df, id="id")

ggplot(gg, aes(x=id, y=variable, fill=value)) +
    geom_tile() +
    scale_fill_gradient(low="#FFFFFF",high="#000000") +
    coord_fixed() +
    theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size=20))
ggsave(file="images/color_matrix.png", height=16, width=16, dpi=300)
