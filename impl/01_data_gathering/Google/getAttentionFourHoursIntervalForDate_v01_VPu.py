#!/usr/local/bin/python3.5


from pytrends.request import TrendReq
import pandas as pd
import datetime
import os

# Login to Google. Only need to run this once, the rest of requests will use the same session.
pytrend = TrendReq()

formatter = "{:02d}".format

databegin = list(map(formatter, range(0, 19, 3)))
dataend   = list(map(formatter, range(4, 25, 3)))

for w in range(306, 373):
    daysprior = w #related to 24/06/2018 - so starting is 21/06/2018
    daysbefore = datetime.date.today() - datetime.timedelta(days=daysprior)

    keywords = ["blockchain"]

    for i in range(0, len(databegin)):
        begin = daysbefore.strftime("%Y-%m-%d") + "T" + databegin[i]
        end   = daysbefore.strftime("%Y-%m-%d") + "T" + dataend[i]

        timeframestring = begin + " " + end

        for j in range (0, len(keywords)):
            pytrend.build_payload(kw_list=[keywords[j]], timeframe=timeframestring)
            df = pytrend.interest_over_time()
            df.to_csv("../data/" + keywords[j] + "/" + timeframestring + ".csv")



    begin = daysbefore.strftime("%Y-%m-%d") + "T21"
    end   = (datetime.date.today() - datetime.timedelta(days=daysprior - 1)).strftime("%Y-%m-%d") + "T01"
    timeframestring = begin + " " + end

    for j in range (0, len(keywords)):
        pytrend.build_payload(kw_list=[keywords[j]], timeframe=timeframestring)
        df = pytrend.interest_over_time()
        df.to_csv("../data/" + keywords[j] + "/" + timeframestring + ".csv")