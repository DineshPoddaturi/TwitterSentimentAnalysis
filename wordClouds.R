
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("readtext")

librarian::shelf(tm, SnowballC, wordcloud, readtext)

library(pdftools)

wordbase <- readtext("DineshPoddaturi-ProposalPacket.pdf")

pdf_text("DineshPoddaturi-ProposalPacket.pdf")

pdftools::pdf_text("DineshPoddaturi-ProposalPacket.pdf")
