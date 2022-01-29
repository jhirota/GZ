
# import pandas, numpy, and matplotlib
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Counterfactual of Economic Effects (sales)------

# Data load (sales)
YamanashiTrend = pd.read_csv("03_build/Counterfactual/output/sales_weekly_data_for_plot.csv")
YamanashiTrend['week'] = pd.to_datetime(YamanashiTrend['week'])

# Data clean (sales)
cofa = YamanashiTrend[YamanashiTrend.type == "counterfactual"]
obs = YamanashiTrend[YamanashiTrend.type == "actual"]
fit = YamanashiTrend[YamanashiTrend.type == "fitted"]
cofa2 = cofa.set_index('week')
cofa2 = cofa2['2020-07-27':'2021-04-30']
obs2 = obs.set_index('week')
obs2 = obs2['2020-03-01':'2021-04-30']
fit2 = fit.set_index('week')
fit2 = fit2['2020-03-01':'2021-04-30']

# Plot (sales)
fig, ax = plt.subplots()
plt.xticks(rotation=45)
ax.plot(cofa2.index, cofa2['sales_per']/10000)
ax.plot(obs2.index, obs2['sales_per']/10000)
ax.plot(fit2.index, fit2['sales_per']/10000, color = "#fcc765")
ax.axvline(pd.to_datetime('2020-07-17'), color='green', linestyle='dashed', lw=2)
ax.text(pd.Timestamp("2020-07-25"), 40, "The GZ certification \n system started.", color = 'green')
plt.legend(['The non-intervention scenario', "The actual sales", "The fitted value of the model"], loc='lower center')
plt.ylabel("Weekly sales per restaurant in Yamanashi Prefecture [10,000 JPY]")
plt.ylim(0, 110)
plt.savefig("04_analyze/Counterfactual/output/cofa_sales.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")

# Counterfactual of Economic Effects (customers)----------

# Data load (customers)
YamanashiTrendCus = pd.read_csv("03_build/Counterfactual/output/customers_weekly_data_for_plot.csv")
YamanashiTrendCus['week'] = pd.to_datetime(YamanashiTrendCus['week'])

# Data clean (customers)
cofaC = YamanashiTrendCus[YamanashiTrendCus.type == "counterfactual"]
obsC = YamanashiTrendCus[YamanashiTrendCus.type == "actual"]
fitC = YamanashiTrendCus[YamanashiTrendCus.type == "fitted"]
cofa2C = cofaC.set_index('week')
cofa2C = cofa2C['2020-07-27':'2021-04-30']
obs2C = obsC.set_index('week')
obs2C = obs2C['2020-03-01':'2021-04-30']
fit2C = fitC.set_index('week')
fit2C = fit2C['2020-03-01':'2021-04-30']

# Plot (customers)
fig, ax = plt.subplots()
plt.xticks(rotation=45)
ax.plot(cofa2C.index, cofa2C['customers_per'])
ax.plot(obs2C.index, obs2C['customers_per'])
ax.plot(fit2C.index, fit2C['customers_per'], color = "#fcc765")
ax.axvline(pd.to_datetime('2020-07-17'), color='green', linestyle='dashed', lw=2)
ax.text(pd.Timestamp("2020-03-01"), 400, "The GZ certification \n system started.", color = 'green')
plt.legend(['The non-intervention scenario', "The actual number of customers", "The fitted value of the model"], loc = "lower center")
plt.ylabel("Number of customers per restaurant \n by week in Yamanashi Prefecture")
plt.ylim(0, 450)
plt.savefig("04_analyze/Counterfactual/output/cofa_customers.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")





# Counterfactual of Infection Prevention Effects ---------

# Data load
YamanashiTrendcovid = pd.read_csv("03_build/Counterfactual/output/cofa_covid.csv")
YamanashiTrendcovid['week'] = pd.to_datetime(YamanashiTrendcovid['week'])

# Data clean
cofacovid = YamanashiTrendcovid[YamanashiTrendcovid.type == "counterfactual"]
obscovid = YamanashiTrendcovid[YamanashiTrendcovid.type == "actual"]
fitcovid = YamanashiTrendcovid[YamanashiTrendcovid.type == "fitted"]
cofacovid2 = cofacovid.set_index('week')
cofacovid2 = cofacovid2['2020-07-27':'2021-04-30']
obscovid2 = obscovid.set_index('week')
obscovid2 = obscovid2['2020-03-01':'2021-04-30']
fitcovid2 = fitcovid.set_index('week')
fitcovid2 = fitcovid2['2020-03-01':'2021-04-30']


# Plot
fig, ax = plt.subplots()
plt.xticks(rotation=45)
ax.plot(cofacovid2.index, cofacovid2['nofcases'])
ax.plot(obscovid2.index, obscovid2['nofcases'])
ax.plot(fitcovid2.index, fitcovid2['nofcases'], color = "#fcc765")
ax.axvline(pd.to_datetime('2020-07-17'), color='green', linestyle='dashed', lw=2)
ax.text(pd.Timestamp("2020-08-01"), 150, "The GZ certification \n system started.", color = 'green')
plt.legend(['The non-intervention scenerio', "The actual new infection cases", "The fitted value of the model"] )
plt.ylabel("The new infection cases in total per week")
plt.savefig("04_analyze/Counterfactual/output/cofa_covid.png", format="png", dpi=250, pad_inches=0.1, bbox_inches="tight")




