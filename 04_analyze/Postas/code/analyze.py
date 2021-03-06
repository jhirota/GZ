# import pandas, numpy, and matplotlib
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# data load 
postas = pd.read_csv("03_build/Postas/output/postas_2019_2021.csv")

# data clean
postas['week'] = pd.to_datetime(postas['week'])

Neighbor = postas[postas.treat == "Neighboring_prefs"]
Neighbor = Neighbor.set_index('week')
Yama = postas[postas.treat == "Yamanashi"]
Yama = Yama.set_index('week')


# plot (sales)
fig, axes = plt.subplots()

plt.xticks(rotation=45)
axes.plot(Neighbor.index, Neighbor['sales_per']/10000, color = "#ff9900")
axes.plot(Yama.index, Yama['sales_per']/10000, color = "#339900")
axes.axvline(pd.to_datetime('2020-07-17'), color='gray', linestyle='dashed', lw=2)
axes.text(pd.Timestamp("2020-03-15"), 120, "The GZ certification \n system started.", color = 'white', backgroundcolor = "gray")
axes.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.legend(['Neighboring five prefectures', "Yamanashi Prefecture"])
#plt.title("Time series of weekly sales per restaurant \n in Yamanashi Prefecture and neighboring 5 prefectures")
plt.xlabel("Week")
plt.ylabel("Weekly sales per restaurant [10,000 JPY]")
plt.savefig("04_analyze/Postas/output/sales_plot.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")



# plot (customers)
fig, axes = plt.subplots()

plt.xticks(rotation=45)
axes.plot(Neighbor.index, Neighbor['customers_per'], color = "#ff9900")
axes.plot(Yama.index, Yama['customers_per'], color = "#339900")
axes.axvline(pd.to_datetime('2020-07-17'), color='gray', linestyle='dashed', lw=2)
axes.text(pd.Timestamp("2020-03-15"), 500, "The GZ certification \n system started.", color = 'white', backgroundcolor = "gray")
axes.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.legend(['Neighboring five prefectures', "Yamanashi Prefecture"])
#plt.title("Time series of number of customers per restaurant by week \n in Yamanashi Prefecture and neighboring 5 prefectures")
plt.xlabel("Week")
plt.ylabel("Number of customers per restaurant by week")
plt.savefig("04_analyze/Postas/output/customers_plot.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")

