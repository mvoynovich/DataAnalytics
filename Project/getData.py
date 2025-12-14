import pandas as pd
from pandas_datareader import wb

# Define countries and time frame
europe = [
  'AUT','BEL','CHE','CZE','DEU','DNK','ESP','FIN','FRA','GBR','GRC',
  'HUN','IRL','ISL','ITA','LTU','LVA','NLD','NOR','POL','PRT','ROU',
  'SVK','SVN','SWE','EST','BGR','CYP','HRV','MLT','SRB','MKD','ALB'
]

start_year = 2000
end_year = 2023

# World Bank indicators 
wb_codes = [
  'NY.GDP.MKTP.KD',
  'ST.INT.RCPT.CD',
  'ST.INT.ARVL',
  'SP.POP.TOTL',
  'SL.UEM.TOTL.ZS',
  'NE.GDI.TOTL.ZS',
  'NE.TRD.GNFS.ZS',
  'FP.CPI.TOTL.ZG',
  'PA.NUS.FCRF',
  'IS.AIR.PSGR'
]

names = [
  'GDP_constant_USD',
  'Tourism_receipts_USD',
  'Tourism_arrivals',
  'Population',
  'Unemployment_rate_pct',
  'Gross_fixed_capital_formation_pctGDP',
  'Trade_openness_pctGDP',
  'Inflation_pct',
  'Exchange_rate_local_to_USD',
  'Air_transport_passengers'
]

df = wb.download(indicator=wb_codes, country=europe, start=start_year, end=end_year)

df = df.reset_index()

rename_dict = dict(zip(wb_codes, names))
df = df.rename(columns=rename_dict)

output_file = "Europe_Tourism_GDP.csv"
df.to_csv(output_file, index=False)

