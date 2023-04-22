library(deSolve)
library(ggplot2)
library(gganimate)

# Define the SIR model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Set the initial values and parameters
init_state <- c(S = 999, I = 1, R = 0)
parameters <- c(beta = 0.5, 
                gamma = 0.1, 
                N = sum(init_state))

# Simulate the model
times <- seq(0, 100, by = 0.1)
sir_out <- ode(y = init_state, 
               times = times, 
               func = sir_model, 
               parms = parameters)

# Create a data frame from the simulation results
sir_df <- data.frame(time = sir_out[,1], 
                     S = sir_out[,2], 
                     I = sir_out[,3], 
                     R = sir_out[,4])

sir_long <- reshape2::melt(sir_df, id.vars = "time")

# Create a ggplot object
sir_plot <- ggplot(sir_long, 
                   aes(x = time, y = value, 
                               color = variable)) +
  geom_line(linewidth = 2) +
  ggthemes::scale_color_fivethirtyeight()+
  labs(x = "Time", y = "Population", color = "") +
  coord_cartesian(xlim = c(0,50))+
  ggthemes::theme_fivethirtyeight()+
  theme(legend.key.size = unit(5,units = "pt"),
        legend.text = element_text(size=22))

# Animate the plot
sir_anim <- sir_plot +
  transition_reveal(time) +
  ease_aes('linear') +
  labs(title = 'Time: {frame_time}') +
  ggtitle('SIR Model Simulation') +
  theme(plot.title = element_text(hjust = 0.5))

# animate(sir_anim, fps = 10, duration = 10)
