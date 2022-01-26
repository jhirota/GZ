
# import pandas, numpy, and matplotlib
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# data load 
covid = pd.read_csv("03_build/weekSIR/output/cases_timeseries_plot.csv")


# data clean
covid['week'] = pd.to_datetime(covid['week'])

Neighbor = covid[covid.treat == "Neighboring_prefs"]
Neighbor = Neighbor.set_index('week')
Yama = covid[covid.treat == "Yamanashi"]
Yama = Yama.set_index('week')


# plot
fig, axes = plt.subplots()

plt.xticks(rotation=45)
axes.plot(Neighbor.index, Neighbor['newcase_day_per'], color = "#ff9900")
axes.plot(Yama.index, Yama['newcase_day_per'], color = "#339900")
axes.axvline(pd.to_datetime("2020-04-07"), color='gray', linestyle='dashed', lw=2)
axes.text(pd.Timestamp("2020-04-07"), 28, "The first \n state of emergency", color = 'gray')
axes.axvline(pd.to_datetime("2020-07-22"), color='gray', linestyle='dashed', lw=2)
axes.text(pd.Timestamp("2020-07-22"), 23, "Go To Campaign", color = 'gray')
axes.axvline(pd.to_datetime("2021-01-07"), color='gray', linestyle='dashed', lw=2)
axes.text(pd.Timestamp("2021-01-07"), 28, "The second \n state of emergency", color = 'gray')
plt.legend(['Neighboring 5 prefectures', "Yamanashi"], loc = "center left")
plt.xlabel("Week")
plt.ylabel("New infection cases per 100,000 population")
plt.title("Time series of new infection cases by week")
plt.ylim(-2.5, 35)
plt.savefig("04_analyze/Covid/output/covid_plot.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")

