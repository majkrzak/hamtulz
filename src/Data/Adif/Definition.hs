module Data.Adif.Definition where

-- | List of QSO fields
-- Fetched from https://www.adif.org/314/ADIF_314.htm#QSO_Fields with:
-- `JSON.stringify([...document.querySelectorAll('#Field_QSO tr > td:first-child')].map(element => element.textContent))`
qsoFields :: [String]
qsoFields = ["ADDRESS","ADDRESS_INTL","AGE","ALTITUDE","ANT_AZ","ANT_EL","ANT_PATH","ARRL_SECT","AWARD_SUBMITTED","AWARD_GRANTED","A_INDEX","BAND","BAND_RX","CALL","CHECK","CLASS","CLUBLOG_QSO_UPLOAD_DATE","CLUBLOG_QSO_UPLOAD_STATUS","CNTY","COMMENT","COMMENT_INTL","CONT","CONTACTED_OP","CONTEST_ID","COUNTRY","COUNTRY_INTL","CQZ","CREDIT_SUBMITTED","CREDIT_GRANTED","DARC_DOK","DISTANCE","DXCC","EMAIL","EQ_CALL","EQSL_QSLRDATE","EQSL_QSLSDATE","EQSL_QSL_RCVD","EQSL_QSL_SENT","FISTS","FISTS_CC","FORCE_INIT","FREQ","FREQ_RX","GRIDSQUARE","GRIDSQUARE_EXT","GUEST_OP","HAMLOGEU_QSO_UPLOAD_DATE","HAMLOGEU_QSO_UPLOAD_STATUS","HAMQTH_QSO_UPLOAD_DATE","HAMQTH_QSO_UPLOAD_STATUS","HRDLOG_QSO_UPLOAD_DATE","HRDLOG_QSO_UPLOAD_STATUS","IOTA","IOTA_ISLAND_ID","ITUZ","K_INDEX","LAT","LON","LOTW_QSLRDATE","LOTW_QSLSDATE","LOTW_QSL_RCVD","LOTW_QSL_SENT","MAX_BURSTS","MODE","MS_SHOWER","MY_ALTITUDE","MY_ANTENNA","MY_ANTENNA_INTL","MY_ARRL_SECT","MY_CITY","MY_CITY_INTL","MY_CNTY","MY_COUNTRY","MY_COUNTRY_INTL","MY_CQ_ZONE","MY_DXCC","MY_FISTS","MY_GRIDSQUARE","MY_GRIDSQUARE_EXT","MY_IOTA","MY_IOTA_ISLAND_ID","MY_ITU_ZONE","MY_LAT","MY_LON","MY_NAME","MY_NAME_INTL","MY_POSTAL_CODE","MY_POSTAL_CODE_INTL","MY_POTA_REF","MY_RIG","MY_RIG_INTL","MY_SIG","MY_SIG_INTL","MY_SIG_INFO","MY_SIG_INFO_INTL","MY_SOTA_REF","MY_STATE","MY_STREET","MY_STREET_INTL","MY_USACA_COUNTIES","MY_VUCC_GRIDS","MY_WWFF_REF","NAME","NAME_INTL","NOTES","NOTES_INTL","NR_BURSTS","NR_PINGS","OPERATOR","OWNER_CALLSIGN","PFX","POTA_REF","PRECEDENCE","PROP_MODE","PUBLIC_KEY","QRZCOM_QSO_UPLOAD_DATE","QRZCOM_QSO_UPLOAD_STATUS","QSLMSG","QSLMSG_INTL","QSLRDATE","QSLSDATE","QSL_RCVD","QSL_RCVD_VIA","QSL_SENT","QSL_SENT_VIA","QSL_VIA","QSO_COMPLETE","QSO_DATE","QSO_DATE_OFF","QSO_RANDOM","QTH","QTH_INTL","REGION","RIG","RIG_INTL","RST_RCVD","RST_SENT","RX_PWR","SAT_MODE","SAT_NAME","SFI","SIG","SIG_INTL","SIG_INFO","SIG_INFO_INTL","SILENT_KEY","SKCC","SOTA_REF","SRX","SRX_STRING","STATE","STATION_CALLSIGN","STX","STX_STRING","SUBMODE","SWL","TEN_TEN","TIME_OFF","TIME_ON","TX_PWR","UKSMG","USACA_COUNTIES","VE_PROV","VUCC_GRIDS","WEB","WWFF_REF"]