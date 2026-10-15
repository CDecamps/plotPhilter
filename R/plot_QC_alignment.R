#' plot_QC_alignment
#'
#' @param count_summary the .counts.summary file
#' @param count_table 
#' @param metadata 
#' @param name 
#'
#' @returns
#' @export
#'
#' @examples
plot_QC_alignment = function(count_summary, count_table, metadata, name = ""){
  
  df_QC = data.frame(samples = metadata$sample,
                     cell_line = metadata$X..cell.line,
                     nb_counts = colSums(count_table),
                     nb_reads_assigned = unlist(count_summary["Assigned",]),
                     nb_reads_not_assigned= unlist(count_summary["Unassigned_NoFeatures",]))
  
  df_QC$nb_reads_total = df_QC$nb_reads_assigned + df_QC$nb_reads_not_assigned
  df_QC$prop_reads_on_peaks = df_QC$nb_reads_assigned / df_QC$nb_reads_total*100
  df_QC$counts_by_reads = df_QC$nb_counts/df_QC$nb_reads_assigned
  
  print(ggplot(df_QC, aes(x = nb_reads_assigned, y = nb_counts, color = cell_line)) + 
          geom_point(size=2) + 
          theme_minimal() + 
          labs(x = "Nb de reads assignés aux pics", 
               y = "Nb de counts (1 read peut être compté plusieurs fois)",
               color = "Cell line", title = name, subtitle = "Vérification de l'impact des fenêtres chevauchantes") )
  
  print(ggplot(df_QC, aes(x = nb_reads_total, y = nb_reads_assigned, color = cell_line)) + 
          geom_point(size=2) + 
          theme_minimal() + 
          labs(x = "Nb de reads alignés",
               y = "Nb de reads assignés aux pics", 
               color = "Cell line", title = name, subtitle = "Corrélation entre reads totaux et reads correspondant aux pics"))
  
  print(ggplot(df_QC, aes(x = cell_line, y = prop_reads_on_peaks, fill = cell_line)) + 
          geom_boxplot() + 
          theme_minimal() + 
          labs(x = "Cell line",
               y = "% des reads assignés aux pics", 
               fill = "Cell line", title = name, subtitle = "% d'alignement sur les pics entre lignées") + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  
  return(df_QC)
}
