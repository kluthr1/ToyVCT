library(shiny)
library(magick)
library(ggplot2)


create_waffle_plot <- function (input_val) {
  i <- 0
  i <- i + 1
  rs <- c()
  cs <- c()
  num <- as.numeric(input_val)
  
  clrs <- c()
  for (v in seq(from = min(0), to = max(99))) {
    rs <- c(rs, v %% 10)
    cs <- c(cs, v %/% 10)
    clrs <- c(clrs, (v + 1) <= num)
  }
  df <- data.frame(rs, cs, clrs)
  as.integer(as.logical(df$clrs))
  plot <- ggplot(df, aes(x = rs, y = cs)) +
    geom_point(
      aes(fill = factor(clrs)),
      size = 2.25,
      colour = "black",
      shape = 21,
      stroke = 1,
    ) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("white", "black"))
    return(plot)
}
create_plot <- function () {
  img1 <- image_read_svg('www/Background_1.svg')

    
  arrows = image_mosaic(c(image_read_svg('www/bot_small.svg'), 
                          image_read_svg('www/top_large.svg'),
                          image_read_svg('www/middle_small.svg')))
  arrows = image_transparent(arrows, 'white')
  img2 <- image_read_svg('www/bot_small.svg')
  img3 <- image_read_svg('www/Background_3.svg')
  out <- image_composite(img1, arrows)
  out <- image_composite(out, img3)
  #out <- image_composite(out, fig, offset = "+190+250")
  return(out)
  
  }
  
generate_plot = function(value, type, av ){
  
  plot <- image_graph(width = 90, height = 90, res = 72)
  print(create_waffle_plot(value))
  dev.off()
  white_left = image_blank(width = 53, height= 90, color = "none")
  white_right = image_blank(width = 53, height= 90, color = "none")
  plot = image_append(c(white_left, plot, white_right))
  text = image_blank(width = 196, height= 60, color = "none")
  text = image_annotate(text, type, font = 'Trebuchet', size = 15, gravity = "center",  location = "+0-17" )
  text = image_annotate(text, av, font = 'Trebuchet', style = "oblique", size = 12, gravity = "center",  location = "+0+0" )
  text = image_annotate(text, paste(value, "%", sep = ""), font = 'Trebuchet', size = 14, gravity = "center",  location = "+0+15" )
  plot2 = image_append(c(plot, text), stack = TRUE)
  return (plot2)
}

add_discharge_percents = function(percents, t_image){
  image= t_image
  image = image_annotate(image, paste(percents[1], "%", sep = ""), font = 'Trebuchet', size = 24, gravity = "northeast", location = "+90+285") 
  image = image_annotate(image, paste(percents[2], "%", sep = ""),  font = 'Trebuchet', size = 24, gravity = "northeast",  location = "+90+427") 
  image = image_annotate(image, paste(percents[3], "%", sep = ""), font = 'Trebuchet', size = 24, gravity = "northeast", location = "+90+570") 
  return (image)
}

add_profile = function(date, operation, profile,  t_image){
  image= t_image
  image = image_annotate(image, "Date Calculated" , location = "+50+50", font = 'Trebuchet', style = "oblique", size = 15, gravity = "northeast") 
  image = image_annotate(image, date, location = "+50+70", font = 'Trebuchet', size = 15, gravity = "northeast") 
  image = image_annotate(image, "Operation" , location = "+50+95", font = 'Trebuchet', style = "oblique", size = 15, gravity = "northeast") 
  image = image_annotate(image, operation , location = "+50+115", font = 'Trebuchet', size = 15, gravity = "northeast") 
  image = image_annotate(image, "Profile" , location = "+50+140", font = 'Trebuchet', style = "oblique", size = 15, gravity = "northeast") 
  image = image_annotate(image, profile , location = "+50+160", font = 'Trebuchet', size = 15, gravity = "northeast") 
  return (image)
}

ui <- shinyUI(
  mainPanel(
    sidebarLayout(
      sidebarPanel(
        downloadButton('downloadImage', 'Download modified image')
      ),
    imageOutput("myImage")
  ))
  
)




server <- shinyServer(function(input, output, clientData) {

 
  output$myImage <- renderImage({
    
    width  <- clientData$output_plot_width
    height <- clientData$output_plot_height
    mysvgwidth <- width/96
    mysvgheight <- height/96
    upperplots = image_append(c(generate_plot(50, "Infection Complications", "Above Average"),
                                generate_plot(20, "Renal (Kidney) Infection", "Above Average"),
                                generate_plot(10, "Cardiac Complications", "Average")
                              ))
    bottomplots = image_append(c(generate_plot(50, "Infection Complications", "Above Average"),
                                 generate_plot(20, "Pulmonary Complications", "Average"),
                                 generate_plot(10, "Cardiac Complications", "Average")
    ))
    final_plot1 = image_composite(create_plot(), upperplots, offset = "+132+242")
    final_plot1 = image_composite(final_plot1, bottomplots, offset = "+132+536")
    temp = add_discharge_percents(c(70, 15, 15), final_plot1)
    temp = add_profile("12/12/12", "55866 - Minimally Invasive Radical Prostatectomy", "Male, Age 74", temp)
    final_plot1 = image_composite(final_plot1, temp)
    tmpfile <- final_plot1 %>% image_write(tempfile(fileext='svg'), format = 'svg')

    # Return a list
    list(src = tmpfile, contentType = "image/svg+xml")
  })
  output$downloadImage <- downloadHandler(
    filename = "Modified_image.jpeg",
    contentType = "image/jpeg",
    content = function(file) {
      ## copy the file from the updated image location to the final download location
      plot2 <- create_waffle_plot(50)
      plot_temp_file <- tempfile(fileext='png')
      ggsave(plot=plot2, filename=plot_temp_file, device='png')
      final_plot <- create_plot()
      print(plot_temp_file)
      plot2 <- image_read(plot_temp_file)
      plot2 <- image_resize(plot2, "100x100")
      final_plot1 <- image_composite(final_plot, plot2, offset = "+190+250")
      tmpfile <- final_plot1 %>%
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      file.copy(tmpfile, file)
    }
  ) 
})
shinyApp(ui = ui, server = server)