library(readr)
library(ggplot2)
library(dplyr)

df <- read_csv("data.csv")

model <- lm(A ~ CONC, data = df)
coefs <- coefficients(model)
intercept <- round(coefs[1], 4)
slope <- round(coefs[2], 4)
r2 <- round(summary(model)$r.squared, 4)

reg_eq <- ifelse(
  slope >= 0,
  paste0("y = ", intercept, " + ", slope, "x\nR² = ", r2),
  paste0("y = ", intercept, " - ", abs(slope), "x\nR² = ", r2)
)

target_A <- 0.258
pred_CONC <- (target_A - intercept) / slope
pred_point <- data.frame(CONC = pred_CONC, A = target_A)

ggplot(df, aes(x = CONC, y = A, color = Set)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  annotate(
    "text",
    x = quantile(df$CONC, 0.7), y = quantile(df$A, 0.8)+0.04,
    label = reg_eq,
    size = 4,
    color = "red",
    fontface = "bold"
  ) +
  geom_point(
    data = pred_point,
    aes(x = CONC, y = A),
    color = "red",
    fill = "red",
    size = 4,
    shape = 21,
    stroke = 1.2,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = pred_point$CONC,
    y = pred_point$A + 0.06,
    label = paste0("A=0.258\nCONC≈", round(pred_point$CONC, 4)),
    color = "blue",
    size = 4,
    hjust = 0.5
  ) +
  labs(
    title = "A and CONC",
    x = "CONC(mg/ml)",
    y = "A",
    color = "Set",
    shape = "Set"
  ) +
  theme_bw()

