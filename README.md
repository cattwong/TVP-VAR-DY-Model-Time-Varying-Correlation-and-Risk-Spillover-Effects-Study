# TVP-VAR-DY Model Time Varying Correlation and Risk Spillover Effects Study
Shanghai Crude Oil Futures and Stock Market: Time-Varying Correlation and Risk Spillover Effects Study Research Based on the TVP-VAR-DY Model
# Shanghai Crude Oil Futures and Stock Market: Time-Varying Correlation and Risk Spillover Effects Study
## Research Based on the TVP-VAR-DY Model

### Project Overview

This project explores the dynamic relationship between the Shanghai crude oil futures market and the Chinese stock market by studying their time-varying correlations and risk spillover effects. Understanding these linkages is crucial for investors to make informed decisions regarding the risks associated with the stock and crude oil futures markets. Additionally, the findings contribute to the enhancement of China’s energy financial system and the acquisition of international oil pricing power.

### Key Features

- **Data Selection**: Analysis of the INE crude oil futures prices and the SSE Composite Index (SSEC) from March 26, 2018, to January 13, 2023, sourced from Tonghuashun.
- **Volatility Analysis**: Calculation of logarithmic returns to ensure data stationarity.
- **Descriptive Statistics**: Comparison of means, standard deviations, skewness, and kurtosis for the selected indicators.
- **GARCH Modeling**: Application of GARCH(1,1) models to analyze the volatility of the three markets.
- **TVP-VAR-DY Model**: Modeling of volatility spillovers between markets using the TVP-VAR-DY model to obtain bi-directional spillover indices.

### Operating Instructions

1. **Import Data**: Start by using the "workdata" original data (interpolated and missing value-filled returns) and import it into the "garch波动率.R" script.
2. **Calculate Volatility**: Run the script to calculate the volatilities of the three markets, and export the results to xlsx files.
3. **Data Consolidation**: Combine the three datasets into "tvpdata.csv".
4. **Model Construction**: Use "tvpvardy.R" to construct the TVP-VAR-DY model and analyze the time-varying spillover effects.

### Empirical Findings

#### Static Analysis of Spillover Effects

The static analysis reveals an average spillover index of 16.7%, indicating that extreme market risks are highly correlated across markets, with significant spillover effects, particularly from the WTI crude oil market to other markets. The Chinese INE crude oil market is significantly influenced by global markets, suggesting a need to strengthen its resilience and international influence.

#### Dynamic Analysis of Spillover Effects

Dynamic analysis highlights the market's sensitivity to extreme events, with notable spillover spikes during major geopolitical and economic events, such as the 2020-2021 COVID-19 pandemic and the 2022 Russia-Ukraine war. The WTI crude oil market remains a dominant force, exerting strong spillover effects on other markets.

### Conclusion

This study concludes that the Shanghai crude oil futures market is highly influenced by global markets, particularly the WTI crude oil market. The findings suggest that to enhance market stability and international influence, the Chinese market should focus on improving its independence and resilience, and continue reforms to strengthen its competitive edge globally.

### Data Sources

- INE Crude Oil Futures Prices
- SSE Composite Index (SSEC)
- Data retrieved from Tonghuashun

### Code Description

- **garch波动率.R**: Script for calculating market volatilities.
- **tvpvardy.R**: Script for constructing and analyzing the TVP-VAR-DY model.

### Contributing

Contributions are welcome! Please submit a Pull Request or report issues.

### Contact

- [GitHub Profile](https://github.com/cattwong)
- Email:wwenxinwang@outlook.com

---

# 上海原油期货和股票市场的时变相关性与风险溢出效应研究
## 基于TVP-VAR-DY模型的研究

### 项目概述

本项目研究了上海原油期货市场与中国股票市场之间的动态关系，重点探讨了它们的时变相关性和风险溢出效应。理解这些联动关系对投资者做出合理的风险规划至关重要，同时也有助于完善我国的能源金融体系，获取国际油价定价权。

### 关键功能

- **数据选取**：分析了2018年3月26日至2023年1月13日期间的INE原油期货价格和上证综指（SSEC），数据来源于同花顺。
- **波动性分析**：计算对数收益率以保证数据的平稳性。
- **描述性统计**：比较所选指标的均值、标准差、偏度和峰度。
- **GARCH建模**：应用GARCH(1,1)模型分析三个市场的波动性。
- **TVP-VAR-DY模型**：利用TVP-VAR-DY模型对各市场的波动率进行建模，获取双向溢出指数。

### 操作说明

1. **导入数据**：首先使用"workdata"原始数据（插值补缺后的收益率）导入“garch波动率.R”脚本。
2. **计算波动率**：运行脚本计算三个市场的波动率，并将结果导出为xlsx文件。
3. **数据汇总**：将三组数据汇总至“tvpdata.csv”文件中。
4. **模型构建**：使用“tvpvardy.R”构建TVP-VAR-DY模型并分析时变溢出效应。

### 实证结果

#### 波动溢出效应静态分析

静态分析表明，平均波动溢出指数为16.7%，这表明市场间极端风险高度相关，尤其是WTI原油市场对其他市场的溢出效应最强。中国INE原油市场受到全球市场的显著影响，需加强其抗风险能力和国际影响力。

#### 波动溢出效应动态分析

动态分析显示市场对极端事件的敏感性较高，尤其是在2020-2021年新冠疫情和2022年俄乌战争期间，溢出指数明显上升。WTI原油市场始终保持强大的溢出效应，对全球市场具有显著影响。

### 结论

研究结论表明，上海原油期货市场高度受到全球市场特别是WTI原油市场的影响。建议中国市场应注重提升其独立性和抗风险能力，通过持续改革增强全球竞争力。

### 数据来源

- INE原油期货价格
- 上证综指（SSEC）
- 数据来源于同花顺

### 代码说明

- **garch波动率.R**：用于计算市场波动率的脚本。
- **tvpvardy.R**：用于构建和分析TVP-VAR-DY模型的脚本。

### 贡献

欢迎贡献代码和提出改进建议。

### 联系方式

- [GitHub 主页](https://github.com/cattwong)
- Email: wwenxinwang@outlook.com
