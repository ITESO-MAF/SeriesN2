
# -- ----------------------------------------------------------------------------------------- -- #
# -- ITESO, Universidad Jesuita de Guadalajara ----------------------------------------------- -- #
# -- Ingeniería Financiera - Departamento de Matematicas y Fisica ---------------------------- -- #
# -- Licencia: GNU General Public License ---------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

SerieDoble <- function(Assets)  {

  MultiData <- data.frame(Assets[,1],Assets[,2]/max(Assets[,2]),Assets[,3]/max(Assets[,3]))
  colnames(MultiData) <- colnames(Assets)
  MultiData <- melt(MultiData, id="TimeStamp", variable.name="Assets", value.name="NormalizedPrice")

  ggsm  <- ggplot(MultiData, aes(x=TimeStamp,y=NormalizedPrice),group=Assets)    +
  geom_line(aes(colour = Assets), linetype = "longdash", size = 1.5)             +
  labs(title = "Stocks", x = "TimeStamp", y = "Normalized Price")                +
  scale_color_manual(values=c("blue","dark grey", "dark blue"))

  y_min <- round(min(MultiData[,3]),1)
  y_max <- round(max(MultiData[,3]),1)
  y_num <- (y_max-y_min)/10

  ggsm1 <<- ggsm + theme(panel.background = element_rect(fill="white"),
  panel.grid.minor.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
  panel.grid.major.y = element_line(size = .25, color = "dark grey", linetype = "solid"),
  panel.grid.major.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
  panel.grid.minor.x = element_line(size = .25, color = "dark grey", linetype = "dashed"),
  axis.text.x  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
  axis.text.y  = element_text(colour = "black",size = 12, hjust =.5,vjust = 0),
  axis.title.x = element_text(colour = "black",size = 16,hjust  =.5,vjust = 0),
  axis.title.y = element_text(colour = "black",size = 16,hjust  =.5,vjust = 1),
  title = element_text(colour = "blue", size = 10, hjust = 1, vjust = 0.8), 
  legend.position = "right", legend.margin = unit(.25, "cm"), 
  legend.background = element_rect(colour = "white", fill = NA),
  legend.text  = element_text(size = 11.5), 
  panel.border = element_rect(linetype = 1, colour = "white", fill = NA),
  legend.key   = element_rect(colour = "white", fill = "white", size = 0.5),
  legend.title = element_blank())  + 
  scale_x_datetime(breaks = datebreaks,labels = date_format("%d/%m/%y %H:%M")) +
  scale_y_continuous(breaks = seq(y_min, y_max, y_num),labels = comma)
return(ggsm1)
}
