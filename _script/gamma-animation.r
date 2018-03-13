library(gganimate)
library(tidyverse)

# Multicollinearity
dta <- map_df(1:15, ~tibble(
  gamma = c(seq(0.1, 0.5, length.out = 6), 
            seq(0.6, 1.2, length.out = 4)),
  lambda = exp(-1 * gamma * (.x - 1))), 
  .id = "variable") %>% 
  mutate(variable = as.numeric(variable))

static_plt <- dta %>% 
  filter(gamma == 0.5) %>% 
  ggplot(aes(variable, lambda)) +
  geom_bar(stat = "identity", position = "identity", fill = "lightgrey") +
  geom_point() + geom_line(group = 1) +
  labs(x = "Variable", y = "Eigenvalues") +
  scale_x_continuous(breaks = unique(dta$variable)) +
  theme(text = element_text(size = 18)) +
  annotate(label = "lambda[i] == e^-gamma(x[i] - 1)", 
           size = 12, family = "serif", geom = "text",
           x = Inf, y = Inf, hjust = 1, vjust = 1, parse = TRUE) +
  annotate(label = "kappa[j] == e^-eta(y[j] - 1)", 
           size = 12, family = "serif", geom = "text",
           x = Inf, y = Inf, hjust = 1, vjust = 2, parse = TRUE)
ggsave(static_plt, filename = "_images/multicollinearity.pdf",
       dpi = 300, width = 7, height = 7)

plt <- ggplot(dta, aes(
  variable, lambda, 
  frame = paste("Degree of multicollinearity:", round(gamma, 2)))) +
  geom_bar(stat = "identity", 
           position = "identity", 
           fill = "lightgrey") +
  geom_point() + geom_line(group = 1) +
  labs(x = "Variable", y = "Eigenvalues") +
  scale_x_continuous(breaks = unique(dta$variable)) +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  annotate(label = "lambda[i] == e^-gamma(x[i] - 1)", 
           size = 12, family = "serif", geom = "text",
           x = Inf, y = Inf, hjust = 1, vjust = 1, parse = TRUE) +
  annotate(label = "kappa[j] == e^-eta(y[j] - 1)", 
           size = 12, family = "serif", geom = "text",
           x = Inf, y = Inf, hjust = 1, vjust = 2, parse = TRUE)
  
gganimate(plt, "_images/multicollinearity.gif", ani.dev = "png")
