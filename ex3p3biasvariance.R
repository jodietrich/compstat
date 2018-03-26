# Computational statistics exercise 3 problem 3 by Jonathan Dietrich

# Bias-variance trade-off

# Non-linear function:
# Inputs: x, 
# Outputs: f(x)
f <- function(x){
  fx <- 0.3* x - 0.2*x^2 + 0.1*x^3 + sin(2*x)
  return(fx)
}

# we are going to use loess smoothers (nonparametric regression), 
#   with different levels of smoothing:
span <- c(0.1,0.2,0.3,0.45,0.7,1)    # smoothing parameter for loess:
# small values mean little smoothing and hence complex models 

# 1a create 25 data sets and do plots for different levels
# I decided to do separate plots for each level of smoothing

# returns y of one simulation
n_sims <- 25
grid <- seq(from=-5,to=5,length=300)

simulate_y <- function(x_values, sd, mean=0){
  y <- f(x_values) + rnorm(n=length(x_values),mean=mean,sd=sd)
  return(y)
}

# takes in x and a matrix of y values (different realizations in columns) and the span parameter
# fits one loess smoother for each column of y_mat with span span_param
matrix_fit_smoothers <- function(x_val, y_mat, span_param){
  smoothers <- apply(y_mat, 2, fit_loess, x=x_val, span_val=span_param)
}

fit_loess <- function(y, x, span_val){
  loess(y ~ x, span=span_val)
}

plot_fitted_curves <- function(fitted_models, model_span){
  plot(x,f(x), type="l", lwd=2, main=paste("alpha=", model_span))
  for (lo in fitted_models){ 
    lines(grid, predict(object=lo, grid),col="blue")
  }
}

run_loess_simulations <- function(sigma, n, n_sims, plot_curves=False){
  y_matrix <- replicate(n = n_sims, expr = simulate_y(x_values = x, sd=sigma))
  
  loess_span_results <- vector("list", length(span))
  for (i in 1:length(span)){
    loess_span_results[[i]] <- matrix_fit_smoothers(x, y_matrix, span[i])
    if(plot_curves){
      plot_fitted_curves(loess_span_results[[i]], span[i])
    }
  }
  return(loess_span_results)
}

predict_all_models <- function(model_list, new_data){
  predictions <- vector(mode = "list", length = length(model_list))
  for(i in seq_along(model_list)){
    predictions[[i]] <- sapply(X = model_list[[i]], FUN = predict, newdata=x_test)
  }
  return(predictions)
}

# plot histograms of prediction for x_test for different models
plot_different_span_histograms <- function(fitted_models, x_test, predictions){
  if(missing(predictions)){
    predictions <- predict_all_models(fitted_models, new_data=x_test)
  }
  for(i in 1:length(fitted_models)){
    curr_span <- span[i]
    hist(predictions[[i]], xlim = c(-2,2), main = paste("alpha=", curr_span))
    abline(v=f(x_test), col="red")
  }
}

# plots a histogram of the predictions for different fitted models in the models vector
plot_prediction_histogram <- function(models, span_param, x_test){
  predictions <- sapply(X = models, FUN = predict, newdata=x_test)
  hist(predictions, xlim = c(-2,2), main = paste("alpha=", span_param))
  abline(v=f(x_test), col="red")
}

mean_squared_error <- function(predictions, true_y){
  mean_error <- mean((predictions - true_y)**2)
  return(mean_error)
}

bias <- function(predictions, true_value){
  bias <- mean(predictions - true_value)
  return(bias)
}

# 1b different values of sigma and n
# parameters for data simulation:
sigma <- 1.5                        # standard devation of noise
n <- 100                            # sample size
x <- seq(from=-5,to=5, length=n)    # x-values (fixed throughout simulation)
run_loess_simulations(sigma, n, n_sims = 25, plot_curves=T)

# 1c plot histograms
sigma <- 1.5                        # standard devation of noise
n <- 100                            # sample size
x_test <- 2
n_sims <- 1000
sim_models <- run_loess_simulations(sigma = sigma, n = n, n_sims = n_sims, plot_curves=FALSE)
predictions <- predict_all_models(sim_models, new_data=x_test)
plot_different_span_histograms(sim_models, x_test=x_test, predictions = predictions)

# 1d test MSE
y_true <- replicate(n = length(sim_models[[1]]), expr = simulate_y(x_values = x_test, sd=sigma))
mse <- sapply(predictions, FUN = mean_squared_error, true_y=y_true)
variance_est <- sapply(predictions, FUN = var)
bias_est <- sapply(predictions, FUN = bias, true_value=f(2))
bias_squared_est <- bias_est**2
plot(span,mse,type="l",col="red", ylim = c(0,6))
lines(span,variance_est,col="green")
lines(span,bias_squared_est,col="blue")
abline(h = sigma**2, lty=2)
legend(x="topleft", lty = c(1, 1, 2), col=c("red", "green", "blue", "black"), legend=c("MSE", "Variance", "Bias**2", "irreducible error"))
bias_squared_est+variance_est+sigma^2-mse
