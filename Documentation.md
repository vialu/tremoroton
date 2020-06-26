
# Welcome to Tremoroton
<br>
<br>
This is an “R” base application for tremor analysis. This application is designed to read 6 channels (2 accelerometers (ACC) and 4 EMG). Once the data is acquired (in another system), it must be transformed into a text file with each channel represented in a separate column and separated by a space before it can be uploaded by Tremoroton (most softwares for data acquisition have the option to export data as .txt).
<br>
For a detailed explanation of the parameters used here and how to interpret the results from this application, refer to: 
<br>
<br>
Vial et al. How to do an electrophysiological study of tremor. Clinical Neurophysiology Practice. Volume 4, 2019, Pages 134-142 
<br>
https://doi.org/10.1016/j.cnp.2019.06.002

<br>
<br>
### General View window:
<br>

In the General View section, specify which column corresponds to which channel by assigning the column numbers and enter the sampling rate in Hz. 
<br>
The ACC channels will be preprocessed with a 3rd order butterworth filter with a high pass at 2 Hz and a low pass at 20Hz.The EMG channels will be preprocessed with a 3rd order butterworth filter with a high pass at 20Hz and a low pass at 350Hz. Also by default, the EMG channels are rectified and smoothed (with Tukey´s smoothing technique). Following these steps, the data can be loaded.
<br>
Plots for each channel will be produced. Specific areas of the plot can be further inspected in more detail with the tools localized in the upper right corner.


<br>
<br>
### Time Domain window:
<br>

Here it is possible to select two channels to perform a more detailed visualization. Below the channel plots there is a two-side scrolling bar that can be used to examine a segment in more detail.

<br>
<br>
### Frequency Domain window:
<br>

By selecting the FFT transformation button (Fast Fourier transformation), FFT spectra of the 6 channels will display in interactive plots.

<br>
<br>
### Coherence window:
<br>

Magnitude squared coherence will calculated between the two channels selected. 
<br>
In order to do this, the data will be divided in segments that will be tapered on the edges. The size of the segments can be determined as a power of two (ej, if the window size is set to 10, that means 210 = 1024 points per segment). The larger the size of the window, the better the frequency resolution. A plot will be displayed with the coherence, and the 95% confidence intervals for significance (based on Halliday et al. (1995)).
<br>
On the right side of the screen there is description of the size of the window used, the frequency resolution and the confidence interval specified. Any time after changing any parameters, the run coherence button should be pressed.

<br>
<br>

### Spectrogram window:

The spectrogram of each channel is obtained by calculating the FFT on approximately 1 second (the next-power-of-2 number of points in 1 second. Eg. For 1000 Hz sampling rate, the window would be 1024 points. This serves to speed the process.) windows with a 50% overlap. The results will be displayed as plots with time on the “x” axis, frequency on the “y” axis and power on the “z” axis (represented as a color scale). This offers a dynamic view of the frequencies over time.


### Contact information

felipevialu@gmail.com
<br>
Please cite: https://doi.org/10.1016/j.cnp.2019.11.004


