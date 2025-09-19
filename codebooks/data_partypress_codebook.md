# Codebook for Party Press Data

This codebook documents all variables in the partypress datasets used for the analysis of "The Green Transition and Political Polarization Along Occupational Lines." The data includes German party press releases with sentiment analysis and frame analysis.

This data is proprietary. Please consult `README.md` for information on how to access this data.

## data_partypress.csv

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **id** | Unique identifier for each press release | PARTYPRESS Database |
| **parlgov_id** | Party identifier from ParlGov database | PARTYPRESS Database |
| **party** | Party abbreviation (e.g., "CDU", "SPD", "AfD") | PARTYPRESS Database |
| **party_name** | Full party name in German | PARTYPRESS Database |
| **party_name_english** | Full party name in English | PARTYPRESS Database |
| **family_name** | Party family classification | PARTYPRESS Database |
| **date** | Date of press release (YYYY-MM-DD) | PARTYPRESS Database |
| **month** | Month of press release | PARTYPRESS Database |
| **month_start** | Start date of month | PARTYPRESS Database |
| **month_end** | End date of month | PARTYPRESS Database |
| **calendar_week** | Calendar week number | PARTYPRESS Database |
| **week_start** | Start date of week | PARTYPRESS Database |
| **week_end** | End date of week | PARTYPRESS Database |
| **issue_multi** | Multi-issue classification | PARTYPRESS Database |
| **issue_mono** | Single issue classification | PARTYPRESS Database |
| **issue_ridge** | Ridge regression issue classification | PARTYPRESS Database |
| **issue_super** | Supervised issue classification | PARTYPRESS Database |
| **issue** | Issue identifier | PARTYPRESS Database |
| **issue_coder2** | Issue classification by second coder | PARTYPRESS Database |
| **position** | Party position on issue | PARTYPRESS Database |
| **position_coder2** | Party position by second coder | PARTYPRESS Database |
| **cv_sample** | Cross-validation sample indicator | PARTYPRESS Database |
| **issue_name** | Issue name (e.g., "Energy", "Environment") | PARTYPRESS Database |
| **text** | Full text content of press release | PARTYPRESS Database |
| **year** | Year of press release | Derived from date |

## data_partypress_sentiment.csv

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **id** | Unique identifier for each press release | PARTYPRESS Database |
| **parlgov_id** | Party identifier from ParlGov database | PARTYPRESS Database |
| **party** | Party abbreviation (e.g., "CDU", "SPD", "AfD") | PARTYPRESS Database |
| **party_name** | Full party name in German | PARTYPRESS Database |
| **party_name_english** | Full party name in English | PARTYPRESS Database |
| **family_name** | Party family classification | PARTYPRESS Database |
| **date** | Date of press release (YYYY-MM-DD) | PARTYPRESS Database |
| **month** | Month of press release | PARTYPRESS Database |
| **month_start** | Start date of month | PARTYPRESS Database |
| **month_end** | End date of month | PARTYPRESS Database |
| **calendar_week** | Calendar week number | PARTYPRESS Database |
| **week_start** | Start date of week | PARTYPRESS Database |
| **week_end** | End date of week | PARTYPRESS Database |
| **issue_multi** | Multi-issue classification | PARTYPRESS Database |
| **issue_mono** | Single issue classification | PARTYPRESS Database |
| **issue_ridge** | Ridge regression issue classification | PARTYPRESS Database |
| **issue_super** | Supervised issue classification | PARTYPRESS Database |
| **issue** | Issue identifier | PARTYPRESS Database |
| **issue_coder2** | Issue classification by second coder | PARTYPRESS Database |
| **position** | Party position on issue | PARTYPRESS Database |
| **position_coder2** | Party position by second coder | PARTYPRESS Database |
| **cv_sample** | Cross-validation sample indicator | PARTYPRESS Database |
| **issue_name** | Issue name (e.g., "Energy", "Environment") | PARTYPRESS Database |
| **text** | Full text content of press release | PARTYPRESS Database |
| **year** | Year of press release | Derived from date |
| **energiewende** | Binary indicator if text contains "Energiewende" | Derived from text analysis |
| **text_clean** | Lowercase version of text | Derived from text preprocessing |
| **text_clean_sw** | Text cleaned and stopwords removed | Derived from text preprocessing |
| **tokens** | List of tokens from text | Derived from text preprocessing |
| **terms** | Number of terms in text | Derived from text preprocessing |
| **anger** | Count of anger emotion words | ED8 Emotion Dictionary |
| **fear** | Count of fear emotion words | ED8 Emotion Dictionary |
| **disgust** | Count of disgust emotion words | ED8 Emotion Dictionary |
| **sadness** | Count of sadness emotion words | ED8 Emotion Dictionary |
| **joy** | Count of joy emotion words | ED8 Emotion Dictionary |
| **enthusiasm** | Count of enthusiasm emotion words | ED8 Emotion Dictionary |
| **pride** | Count of pride emotion words | ED8 Emotion Dictionary |
| **hope** | Count of hope emotion words | ED8 Emotion Dictionary |
| **anger_share** | Share of anger words in total terms | Derived from emotion counts |
| **fear_share** | Share of fear words in total terms | Derived from emotion counts |
| **disgust_share** | Share of disgust words in total terms | Derived from emotion counts |
| **sadness_share** | Share of sadness words in total terms | Derived from emotion counts |
| **joy_share** | Share of joy words in total terms | Derived from emotion counts |
| **enthusiasm_share** | Share of enthusiasm words in total terms | Derived from emotion counts |
| **pride_share** | Share of pride words in total terms | Derived from emotion counts |
| **hope_share** | Share of hope words in total terms | Derived from emotion counts |
| **positive2** | Sum of positive emotions (joy + enthusiasm + pride + hope) | Derived from emotion counts |
| **negative2** | Sum of negative emotions (anger + fear + disgust + sadness) | Derived from emotion counts |
| **polarity2** | Sentiment polarity (positive2 - negative2) | Derived from emotion counts |
| **polarity2.norm** | Normalized sentiment polarity (polarity2 / terms) | Derived from emotion counts |

## data_partypress_frames.csv

| Variable Name | Description | Data Source |
|---------------|-------------|-------------|
| **year** | Year of press releases | Derived from date |
| **Economic_AfD** | Share of economic frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Economic_Other Parties** | Share of economic frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Labor Market_AfD** | Share of labor market frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Labor Market_Other Parties** | Share of labor market frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Energy Security_AfD** | Share of energy security frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Energy Security_Other Parties** | Share of energy security frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Sovereignty_AfD** | Share of sovereignty frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Sovereignty_Other Parties** | Share of sovereignty frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Identity_AfD** | Share of identity frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Identity_Other Parties** | Share of identity frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Climate_AfD** | Share of climate frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Climate_Other Parties** | Share of climate frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Urgency_AfD** | Share of urgency frame words in AfD press releases (%) | Custom Frame Dictionary |
| **Urgency_Other Parties** | Share of urgency frame words in other parties' press releases (%) | Custom Frame Dictionary |
| **Economic_weighted_AfD** | Weighted economic frame score for AfD (frame share × sentiment) | Derived from frame analysis and sentiment |
| **Economic_weighted_Other Parties** | Weighted economic frame score for other parties | Derived from frame analysis and sentiment |
| **Labor Market_weighted_AfD** | Weighted labor market frame score for AfD | Derived from frame analysis and sentiment |
| **Labor Market_weighted_Other Parties** | Weighted labor market frame score for other parties | Derived from frame analysis and sentiment |
| **Energy Security_weighted_AfD** | Weighted energy security frame score for AfD | Derived from frame analysis and sentiment |
| **Energy Security_weighted_Other Parties** | Weighted energy security frame score for other parties | Derived from frame analysis and sentiment |
| **Sovereignty_weighted_AfD** | Weighted sovereignty frame score for AfD | Derived from frame analysis and sentiment |
| **Sovereignty_weighted_Other Parties** | Weighted sovereignty frame score for other parties | Derived from frame analysis and sentiment |
| **Identity_weighted_AfD** | Weighted identity frame score for AfD | Derived from frame analysis and sentiment |
| **Identity_weighted_Other Parties** | Weighted identity frame score for other parties | Derived from frame analysis and sentiment |
| **Climate_weighted_AfD** | Weighted climate frame score for AfD | Derived from frame analysis and sentiment |
| **Climate_weighted_Other Parties** | Weighted climate frame score for other parties | Derived from frame analysis and sentiment |
| **Urgency_weighted_AfD** | Weighted urgency frame score for AfD | Derived from frame analysis and sentiment |
| **Urgency_weighted_Other Parties** | Weighted urgency frame score for other parties | Derived from frame analysis and sentiment |
| **total_tokens_AfD** | Total number of tokens in AfD press releases | Derived from text analysis |
| **total_tokens_Other Parties** | Total number of tokens in other parties' press releases | Derived from text analysis |
| **Economic_diff** | Difference in economic frame share (AfD - Other Parties) | Derived from frame analysis |
| **Labor Market_diff** | Difference in labor market frame share (AfD - Other Parties) | Derived from frame analysis |
| **Energy Security_diff** | Difference in energy security frame share (AfD - Other Parties) | Derived from frame analysis |
| **Sovereignty_diff** | Difference in sovereignty frame share (AfD - Other Parties) | Derived from frame analysis |
| **Identity_diff** | Difference in identity frame share (AfD - Other Parties) | Derived from frame analysis |
| **Climate_diff** | Difference in climate frame share (AfD - Other Parties) | Derived from frame analysis |
| **Urgency_diff** | Difference in urgency frame share (AfD - Other Parties) | Derived from frame analysis |
| **Economic_weighted_diff** | Difference in weighted economic frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Labor Market_weighted_diff** | Difference in weighted labor market frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Energy Security_weighted_diff** | Difference in weighted energy security frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Sovereignty_weighted_diff** | Difference in weighted sovereignty frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Identity_weighted_diff** | Difference in weighted identity frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Climate_weighted_diff** | Difference in weighted climate frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **Urgency_weighted_diff** | Difference in weighted urgency frame score (AfD - Other Parties) | Derived from frame analysis and sentiment |
| **token_ratio** | Ratio of AfD tokens to other parties' tokens | Derived from token counts |