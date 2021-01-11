# TODO: 
#   1. Remove tendency towards teal / light blue
#   2. Make for loops into applys
#   3. Comment

library(ggplot2)
library(magrittr)

width <- 100
height <- 100
colour_vec <- sample(seq(0,255), 3)
number_of_lines <- 50
normal_space <- height / number_of_lines
lines <- vector(mode = "list", length = number_of_lines)
colour_vecs <- vector(mode = "list", length = number_of_lines)

sample_and_jitter_colour <- function(colour_vec, selected_rgb_index_list){
  jittered_rgb_val <- jitter(c(colour_vec[selected_rgb_index_list[1]]), amount = 25)[1]
  jittered_rgb_val <- round(jittered_rgb_val, 0)
  if (jittered_rgb_val >= 255){
    jittered_rgb_val <- 255
  }
  else if (jittered_rgb_val <= 0){
    jittered_rgb_val <- 0
  }
  new_colour_vec <- colour_vec
  new_colour_vec[selected_rgb_index_list[1]] <- jittered_rgb_val
  if (all(new_colour_vec == colour_vec)){
    return(sample_and_jitter_colour(colour_vec, selected_rgb_index_list[2:length(selected_rgb_index_list)]))
  }
  else{
    return(new_colour_vec)
  }
  
}

for (line_number in seq(number_of_lines+1)){
  # jitter colors
  selected_rgb_index_list <- sample(c(1,2,3), 3)
  colour_vec <- sample_and_jitter_colour(colour_vec, selected_rgb_index_list)
  
  if (line_number > 1 && line_number < number_of_lines+1){
    # jitter line
    line <- line+normal_space
    line <- jitter(line, amount = .25)
  }
  else if (line_number == 1){
    line <- rep(0, width)
  }
  else if (line_number == number_of_lines+1)
    line <- rep(height, width)
  
  lines[[line_number]] <- I(line)
  colour_vecs[[line_number]] <- colour_vec
}

add_line <- function(plot, line_df, colour) {
  return (plot + 
            geom_smooth(aes(x=unlist(line_df['x']), y=unlist(line_df['y'])), method="loess", se=FALSE, fullrange=TRUE, colour = "black") +
            geom_ribbon(aes(x=unlist(line_df['x']), ymin=unlist(line_df['y']), ymax=unlist(line_df['next_y'])), fill=colour)+
            theme_void()
  )
}

p <- ggplot() +
  theme_void()
for (line_number in seq_along(lines)){
  if (line_number < length(lines)){
    line_df <- data.frame(y = lines[[line_number]], x = seq(from = 0, to=length(lines[[line_number]])-1, by = 1), 
                          next_y = lines[[line_number+1]])
    colour_vec <- colour_vecs[line_number]
    colour <- rgb(colour_vec[[1]][1], colour_vec[[1]][2], colour_vec[[1]][2], maxColorValue = 255)
    p <- p +
      geom_ribbon(data = line_df, aes(x=x, ymin=y, ymax=next_y), fill=colour)
      
  }
}
p

