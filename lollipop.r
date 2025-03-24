# ----------------------------------- lollipop code
# this uses ggplot2 to create a nice looking lollipop chart of selected GO terms, which were selected after running a GO enrichment analysis
https://r-charts.com/ranking/lollipop-chart-ggplot2/ - ggplot - what I used as a base for the lollipop chart below
https://amigo.geneontology.org/amigo/search/ontology - AmiGO - what I used to look up terms to get total number of genes

# 1_PPVvsControl_ROSOonly_UP.csv
IN <- read.csv("chosen_GO_terms.csv")
IN=IN[rev(order(IN$FDR)),]  
df <- data.frame(
  x = c(as.factor(1:10)), # in order to make sure labels line up with data, need to put this here - will need to modify if you have more/less than 10 terms
  y = IN$Fold.enrichment,
  z = IN$gene_number,
  p_value = as.numeric(IN$FDR),  # p-value column
  GOlabels=IN$X
)

library(ggplot2)
dev.new(width=8.5,height=3.5)

# the original plot - ordered by FDR q-value and black colour for circles removed
ggplot(df, aes(x = x, y = y, color = p_value)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y), size = 1) +  
  geom_point(aes(size = z, fill = p_value), pch = 21) +  
  geom_text(aes(label = z), color = "white", size = 3) +
  scale_x_discrete(labels = df$GOlabels) +
  coord_flip() +
  labs(x = "GO Accession, Name", 
       y = "Fold Enrichment", 
       color = "FDR q-value", 
       size = "Genes", 
       fill = "FDR q-value") +
  scale_size(range = c(5, 10), breaks = c(5, 7.5, 10)) +  # Control legend intervals
  scale_color_gradient(low = "red", high = "black") +
  scale_fill_gradient(low = "red", high = "black") +
  theme_minimal()
