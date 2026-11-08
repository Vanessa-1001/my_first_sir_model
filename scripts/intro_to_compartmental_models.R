

#Load necessary packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(deSolve,
               tidyverse,
               here
               )
 

#Setting up the SIR Model in R----

  #STEP 1: Set up initial conditions and parameters
#Create vector/variable called initial state
initial_state <- c(S = 762, I = 1, R = 0)
initial_state

#Define the values of transmission (beta) and recovery (gamma) parameters
 #Create vector/variable parameters
parameters <- c(beta = 0.0026,
                gamma = 0.5)
parameters

#Set the time span for my simulation
time <- seq(1, 14, by = 1) #interval in days, 1 /// seq is first and last value 1, 14
time

  #STEP 2: Write the SIR Model function
#Define function and arguments
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {

    #Rate of change
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    #Return the rate of change
    list(c(dS, dI, dR))
  })
}
   
  #STEP 3: Solve the differential equations using deSolve and ODE function
#create object with solutions for differential equations
sir_out <- ode(
  y = initial_state,
  time = time,
  func = sir_model,
  parms = parameters
)

sir_out


  #STEP 4: Convert the output to a data frame
sir_out <- as.data.frame(sir_out)
sir_out

#VISUALIZE THE RESULTS
 #create a plot to visualize the dynamics of th SIR model
ggplot(sir_out, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible")) +
  geom_line(aes(y = I, color = "Infected")) +
  geom_line(aes(y = R, color = "Recovered")) +
  labs(title = "SIR Model Simulation",
       x = "Time (days)",
       y = "Population Count") +
  scale_color_manual(values = c("Susceptible" = "#2F5597", "Infected" = "#C00000",
                                "Recovered" = "#548235")) +
  theme_minimal()
  

#Load observed data
observed_data <- read.csv(here("data-raw/influenza_boarding_school.csv"))
observed_data 
  
#Create a comparison graph
comparison_graph <- 
  ggplot() +
  geom_line(data = observed_data,
            aes(x = day, y = cases),
            color = "black") +
  geom_point(data = observed_data,
             aes(x = day, y = cases),
             color = "black") +
  geom_line(data = sir_out,
            aes(x = time, y = I),
            color = 'red') +
  theme_bw()

#load the graph  
comparison_graph



 
  
  
  














