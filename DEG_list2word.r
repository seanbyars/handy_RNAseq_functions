# ----------------------------------- print DEG table to word doc for journal submission
# this code takes a list of DEGs, cleans and prints out as a word document
# this saves having to do this manually in Excel and Word which can take some time
# it also formats the values (p-values, log2FC etc) so they take up less space
# this can be handy if the DEG list(s) from an RNAseq need to be supplied in word rather than excel
library(flextable)
library(officer)
library(dplyr)

# Read the DEG list and simplify so it fits nicely in a word doc
deg_data <- read.csv("dge_output.csv")
deg_data=deg_data[,c("EnsemblId","gene_name","biotype","logFC","P.value","FDR")]
names(deg_data)=c("Ensembl_Id","gene","biotype","log2FC","p","FDR")

# simplify biotype column
deg_data$biotype[c(grep("pseudogene",deg_data$biotype))]="pseudogene" # if using Ensembl annotation, pseudogenes may be labelled with 'transcribed_processed_pseudogene', 'transcribed_unprocessed_pseudogene', 'transcribed_unitary_pseudogene' etc, these take up too much column width - convert all to 'pseudogene'
deg_data$biotype[c(grep("protein_coding",deg_data$biotype))]="PC" # likewise, protein_coding can be converted to 'PC'

# simplify p and FDR values, so it does not print out values with excessive number of zeros
deg_data$p <- formatC(deg_data$p, format = "e", digits = 4)
deg_data$FDR <- formatC(deg_data$FDR, format = "e", digits = 4)

# truncating log2FC values as well to simplify
truncate_decimals <- function(x, digits) {
  factor <- 10^digits
  return(floor(x * factor) / factor)
}
deg_data$log2FC=truncate_decimals(deg_data$log2FC, 4)

# Convert to flextable - this may take a minute
deg_table <- flextable(deg_data) %>%
  autofit() %>%  # Adjusts column width
  theme_vanilla()  # Apply a simple theme

# Create a Word document and add the table - this may take a minute
doc <- read_docx() %>%
  body_add_flextable(deg_table)

# Save to a Word file - this may take a few minutes
print(doc, target = "DEG_list_as_word_doc.docx")
