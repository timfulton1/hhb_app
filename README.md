# Deoxyhemoglobin Kinetics Analysis Application

This repo contains the code used to build the analysis application.

Visit the app here: https://timfulton.shinyapps.io/HHb_Analysis_App/

The app is optimized for Safari, Firefox, and Chrome, but may render slightly differently in each browser if using Windows OS.

<img width="1774" alt="HHB App" src="https://github.com/user-attachments/assets/4c7b4202-6d11-4eef-a75e-54e0ca8ef003">

### Background

I developed this application during my postdoctoral fellowship to enhance the efficiency and accuracy of deoxyhemoglobin (HHb) kinetics analyses. The app performs kinetic modeling of HHb data obtained using near-infrared spectroscopy. The metrics that are calculated provide estimates of microvascular function during the transition from rest to exercise. 


### Methods

The data are fit using a monoexponential model with a time delay according to the equation below:

$$
HHb = {HHb}_{baseline} + \Delta{HHb} \cdot (1 - e^{[-(t - TD) / \tau]})
$$

where `HHb` is the HHb at any time `t`, `baseline HHb` is the average HHb during the 30 seconds of rest prior to exercise, `delta HHb` is the difference between the baseline and the steady state amplitude at 60 seconds, `t` is the time, `TD` is the time delay, and `tau` is the time taken to reach 63% of the steady state amplitude. 

Three additional variables that are calculated are:

1. **Response Time** - The sum of the time delay and the tau.
2. **Overshoot** - The difference between the amplitude and the average HHb over the final 15 seconds of exercise.
3. **RMSE** - The root mean squared error of the fit.

The timespan of the data used will range from the first fitting point to 60 seconds.

## Usage

Upload data using the browse button (visitors can use the demo data). The first fitting point defaults to 6 seconds, but it should be adjusted to the first data point after time zero (exercise start) that is higher than the baseline. The first fitting point can be adjusted using the slider on the right.
