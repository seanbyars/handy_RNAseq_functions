# ----------------------------------- volcano ggplot2 code
# this uses ggplot2 to create a nice looking volcano plot
# some extra code added (e.g. which gene labels to display) for further customisation
http://bioinformatics.sdstate.edu/go/ - shiny app, but also nice examples of standard DEG plots

# load example data
DGEresultsToPlot=read.csv("dge_output.csv")
DGEresultsToPlot$log10FDR=-log10(DGEresultsToPlot$FDR)

# ---  ggplot volcano
library(ggplot2)
library(ggrepel)
library(dplyr)

# --- preprocess - this just add some extra columns that the ggplot function uses
results <- DGEresultsToPlot %>%
  mutate(regulation = case_when(
    logFC >= 1 & FDR < 0.05 ~ "up", # if you don't want the logFC cutoff, use code below
    logFC <= -1 & FDR < 0.05 ~ "down",
    TRUE ~ "neutral"
  ))

#results <- DGEresultsToPlot %>% # use this if you don't want a log2FC cutoff in your volcano plot
#  mutate(regulation = case_when(
#    logFC > 0 & FDR < 0.05 ~ "up", # if you don't want the logFC cutoff, modify this
#    logFC < 0 & FDR < 0.05 ~ "down",
#    TRUE ~ "neutral"
#  ))

table(results$regulation)
#  down neutral      up 
#     48   15029     171 

# this creates and extra dataframe that ggplot uses to add labels to the plot for genes you want to highlight   
# modify this depending on how many you want
top_genes <- results %>%
  arrange(FDR) %>%
  filter(row_number() <= 20) # just take the top 20
rownames(top_genes)=top_genes$gene_name
top_genes$gene=row.names(top_genes)

# removing those that are not FDR sig and >logFC 1
top_genes=top_genes[c(which(top_genes$FDR<0.05 & abs(top_genes$logFC) >= 1)),] 

# sometimes there may be some genes that are not in the top 20 that you may still want to highlight
# use this code to add those
# extra=results[c(which(results$gene_name == "RRAD" | results$gene_name == "DUSP26")),]
# extra$gene=extra$gene_name
# top_genes=rbind(top_genes,extra)

# dev.new(width=6,height=5) # use if you want specific plot height/width
ggplot(results, aes(x=logFC, y=log10FDR, col=regulation)) +
  # Add vertical line at x = 0 (darker)
  geom_vline(xintercept=0, color="black", linetype="solid", size=0.2) +
  # Add horizontal line at y = 0 (darker)
  geom_hline(yintercept=0, color="black", linetype="solid", size=0.2) +
  geom_point(shape=21, fill=NA, stroke=0.5, size=2.3, color="black") +  # Faint outline around the points
  # this plots the grey first, then the blue/red after ensuring the up/downreg genes are on top
  geom_point(data = subset(results, regulation == "neutral"), aes(col=regulation), alpha=0.8, size=2) +
  # Then, plot red (up) and blue (down) points on top
  geom_point(data = subset(results, regulation != "neutral"), aes(col=regulation), alpha=0.8, size=2) +
  scale_colour_manual(values=c('up'="red", 'down'="blue", 'neutral'="grey")) + 
  theme_bw() +
  ggtitle("add title here") +
  # add the x and y axis labels in the appropriate format
  xlab(expression(log[2] * FC)) +  
  ylab(expression(-log[10] * FDR)) +  
  geom_text_repel(data=top_genes, aes(label=gene), 
  size=3, box.padding=0.5, point.padding=0.5, force=20) 
