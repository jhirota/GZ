import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# data load 
weekSIR = pd.read_csv("03_build/weekSIR/output/weekSIR.csv")

# data clean
weekSIR = weekSIR[weekSIR["pref"] == "Yamanashi"][["week", "cumGZ"]].iloc[20:] # since June 2020

series = pd.Series(weekSIR['cumGZ'], dtype='float') 
series.index = pd.to_datetime(weekSIR['week']) 

# plot 
series.plot(color = "green")
# plt.axvline("2020-07-17", color = 'green', linestyle = '--')
plt.annotate("The first expansion",
             xytext = ("2020-06-25", 1500),
             backgroundcolor = "lightgreen",
             xy = ("2020-09-25", 500),
             arrowprops = dict(arrowstyle='-|>'))
plt.annotate("The second expansion",
             xytext = ("2020-12-25", 1000),
             backgroundcolor = "lightgreen",
             xy = ("2021-02-25", 2500),
             arrowprops = dict(arrowstyle='-|>'))
plt.xlabel('Week')
plt.ylabel('Number of the GZ-certified restaurants')
plt.title("Time series of cumulative number of the GZ-certified restaurants")
plt.savefig("04_analyze/GZlist/output/cumulative_GZ_plot.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")
