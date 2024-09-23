#' Full data collected on Smarties
#'
#' Colour frequencies in a sample of 2 packs of Smarties
#'
#' @format
#' A data frame with 2 rows and 8 columns
#' Each row corresponds to a pack of Smarties
#' The columns give the counts of Smarties of each colour per pack
#'
#' @source {in-house}
#' @examples
#' apply(activity7_smarties_sample, 1, sum) # counts number of Smarties per pack
#'
#'ss <- t(activity7_smarties_sample) %>% as.data.frame
#'ss <- cbind(rownames(ss), ss)
#'colnames(ss) <- c('colour', 'box1', 'box2')
#'ss <- reshape2::melt(ss)
#'ggplot2::ggplot(ss, ggplot2::aes(x = colour, y = value, fill = variable)) +
#'    ggplot2::geom_bar(stat="identity", position = ggplot2::position_dodge()) +
#'    ggplot2::geom_text(ggplot2::aes(label = value), vjust= 1.6, color="white",
#'                       position = ggplot2::position_dodge(0.9), size = 3.5) +
#'    ggplot2::scale_fill_brewer(palette = "Paired") +
#'    ggplot2::theme_minimal() +
#'    ggplot2::xlab('Colour') +
#'    ggplot2::ylab('Count')

"activity7_smarties_sample"
