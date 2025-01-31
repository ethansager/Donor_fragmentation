// 1. Read raw dataset. This dataset includes all surveys and all questions after the harmonization of regional classifications. 

use "Raw Dataset.dta", clear

// 2. Prepare questions - scale 0-1 and make sure all questions are in the direction of higher value = lower corruption.

// First, we remove questions that are unusable or too unique for meaningful global comparisons.
drop WBES_corr12 Afro7_Q45 Afro7_Q46 Afro7_Q53B Afro7_Q56K Afro_corrfirstprob Afro_corrsecprob Afro_corrthirdprob Afro_bribereport Latin_corrprivate Latin_corrparliamentyesno AFTimor_m46_5 AFTimor_m47_12 Latin_corrjudgeyesno Latin_corrtaxofficialsyesno Latin_taxcorr Latin_firstproblem Asia_corrprob TIDRC_corrprob WBES_corr11 Latin_corrpoliceyesno Latin_corrgovcouncilyesno Latin_expercorr Latin_corrwitness WVS_mn_228q Asia_q120 MAsia_q119 Afro7_Q49R Afro8_Q44L QoG2017_Q17_3 QoG2021_q18_3 Afro_bribecourt QoG2013_q17 QoG2017_Q16_1 QoG2021_q17_1 LAPOP_parh150 QoG2017_Q17_1 QoG2021_q18_1 Afro7_Q47 Afro7_Q49N QoG2013_q13 QoG2013_q14 QoG2013_q16_4 QoG2017_Q13 QoG2017_Q14 QoG2017_Q16_2 QoG2017_Q17_4 QoG2017_Q18_4 QoG2021_q14 QoG2021_q15 QoG2021_q17_2 QoG2021_q18_4 QoG2021_q19_4 LAPOP_exc13 LAPOP_exc20 AFTimor_m46_1 AFTimor_m47_02 AFTimor_m46_4 AFTimor_m47_06 TI_Q6_12 TI_Q24_1 TI_Q24_2 TI_Q24_3 TI_Q24_5 TI_Q24_6 AFTimor_m47_08 AFAfghan_x23a AFAfghan_x23b AFAfghan_x23d WBES_obst4 WBES_graft2 WBES_graft3 LAPOP_pr3e WBES_corr3 AFTimor_m46_2 Latin_corrgovoffyesno WBPOS_correffail WBPOS_fragcorr WBPOS_corrresource WBPOS_corrgrowth WBPOS_corresearch WBPOS_corrshared Q48A Q48B Q48C Q48D Q48E Q48F Q2b Q6_8 Q6_9 Q6_11 Q6_13 Q6_14 Q19c TIDRC_corrprob CORRPEOPLE8FIN CORRPEOPLE9FIN CORRPEOPLE10FIN CORRPEOPLE16FIN xBRIBETOTEXCFIN

replace ISSP_v60 = . if ISSP_v60 == 8 | ISSP_v60 == 9
replace ISSP_v60 = ((ISSP_v60 * -1) + 5) / 4

replace ISSP_v61 = . if ISSP_v61 == 8 | ISSP_v61 == 9
replace ISSP_v61 = ((ISSP_v61 * -1) + 5) / 4

replace ISSP_v62 = . if ISSP_v62 == 8 | ISSP_v62 == 9
replace ISSP_v62 = ((ISSP_v62 * -1) + 5) / 4

replace Euro1_corrprob = . if Euro1_corrprob == 5
replace Euro1_corrprob = (Euro1_corrprob - 1) / 3

replace Euro1_corrlocgov = . if Euro1_corrlocgov == 5
replace Euro1_corrlocgov = (Euro1_corrlocgov - 1) / 3

replace Euro1_corrnatgov = . if Euro1_corrnatgov == 5
replace Euro1_corrnatgov = (Euro1_corrnatgov - 1) / 3

rename Euro1_corrpolice Euro1_corrpoliceyesno
replace Euro1_corrpoliceyesno = (Euro1_corrpoliceyesno * -1) + 1

rename Euro1_corrjudge Euro1_corrjudgeyesno
replace Euro1_corrjudgeyesno = (Euro1_corrjudgeyesno * -1) + 1

rename Euro1_corrpolitician Euro1_corrpoliticianyesno
replace Euro1_corrpoliticianyesno = (Euro1_corrpoliticianyesno * -1) + 1

rename Euro1_bribepolice Euro1_bribepoliceyesno
replace Euro1_bribepoliceyesno = 0 if Euro1_bribepoliceyesno == 9
replace Euro1_bribepoliceyesno = (Euro1_bribepoliceyesno * -1) + 1

rename Euro1_bribejudge Euro1_bribejudgeyesno
replace Euro1_bribejudgeyesno = 0 if Euro1_bribejudgeyesno == 9
replace Euro1_bribejudgeyesno = (Euro1_bribejudgeyesno * -1) + 1

rename Euro1_bribehealth Euro1_bribehealthyesno
replace Euro1_bribehealthyesno = 0 if Euro1_bribehealthyesno == 9
replace Euro1_bribehealthyesno = (Euro1_bribehealthyesno * -1) + 1

rename Euro1_bribeeduc Euro1_bribeeducyesno
replace Euro1_bribeeducyesno = 0 if Euro1_bribeeducyesno == 9
replace Euro1_bribeeducyesno = (Euro1_bribeeducyesno * -1) + 1

rename Euro1_corrgovoff Euro1_corrgovoffyesno
replace Euro1_corrgovoffyesno = (Euro1_corrgovoffyesno * -1) + 1

rename Euro1_bribegovoff Euro1_bribegovoffyesno
replace Euro1_bribegovoffyesno = (Euro1_bribegovoffyesno * -1) + 1

replace Euro2_corrprob = . if Euro2_corrprob == 6
replace Euro2_corrprob = 4 if Euro2_corrprob == 5
replace Euro2_corrprob = (Euro2_corrprob - 1) / 3

replace Euro2_corrlocgov = 4 if Euro2_corrlocgov == 5
replace Euro2_corrlocgov = (Euro2_corrlocgov - 1) / 3

replace Euro2_corrnatgov = 4 if Euro2_corrnatgov == 5
replace Euro2_corrnatgov = (Euro2_corrnatgov - 1) / 3

rename Euro2_corrpolice Euro2_corrpoliceyesno
replace Euro2_corrpoliceyesno = (Euro2_corrpoliceyesno * -1) + 1

rename Euro2_corrtax Euro2_corrtaxyesno
replace Euro2_corrtaxyesno = (Euro2_corrtaxyesno * -1) + 1

rename Euro2_corrjudge Euro2_corrjudgeyesno
replace Euro2_corrjudgeyesno = (Euro2_corrjudgeyesno * -1) + 1

rename Euro2_corrpolitician Euro2_corrpoliticianyesno
replace Euro2_corrpoliticianyesno = (Euro2_corrpoliticianyesno * -1) + 1

rename Euro2_corrgovoff Euro2_corrgovoffyesno
replace Euro2_corrgovoffyesno = (Euro2_corrgovoffyesno * -1) + 1

rename Euro2_bribepolice Euro2_bribepoliceyesno
replace Euro2_bribepoliceyesno = 0 if Euro2_bribepoliceyesno == 9 | Euro2_bribepoliceyesno == 99
replace Euro2_bribepoliceyesno = (Euro2_bribepoliceyesno * -1) + 1

rename Euro2_bribejudge Euro2_bribejudgeyesno
replace Euro2_bribejudgeyesno = 0 if Euro2_bribejudgeyesno == 9 | Euro2_bribejudgeyesno == 99
replace Euro2_bribejudgeyesno = (Euro2_bribejudgeyesno * -1) + 1

rename Euro2_bribehealth Euro2_bribehealthyesno
replace Euro2_bribehealthyesno = 0 if Euro2_bribehealthyesno == 9 | Euro2_bribehealthyesno == 99
replace Euro2_bribehealthyesno = (Euro2_bribehealthyesno * -1) + 1

rename Euro2_bribeeduc Euro2_bribeeducyesno
replace Euro2_bribeeducyesno = 0 if Euro2_bribeeducyesno == 9 | Euro2_bribeeducyesno == 99
replace Euro2_bribeeducyesno = (Euro2_bribeeducyesno * -1) + 1

rename Euro2_bribegovoff Euro2_bribegovoffyesno
replace Euro2_bribegovoffyesno = 0 if Euro2_bribegovoffyesno == 9 | Euro2_bribegovoffyesno == 99
replace Euro2_bribegovoffyesno = (Euro2_bribegovoffyesno * -1) + 1

replace Afro_corrpresoff = . if Afro_corrpresoff == -1 | Afro_corrpresoff == 9 | Afro_corrpresoff == 99
replace Afro_corrpresoff = ((Afro_corrpresoff / 3) * -1) + 1

replace Afro_corrparl = . if Afro_corrparl == -1 | Afro_corrparl == 9 | Afro_corrparl == 99
replace Afro_corrparl = ((Afro_corrparl / 3) * -1) + 1

replace Afro_corrloccouncil = . if Afro_corrloccouncil == -1 | Afro_corrloccouncil == 9 | Afro_corrloccouncil == 99
replace Afro_corrloccouncil = ((Afro_corrloccouncil / 3) * -1) + 1

replace Afro_corrgovofficials = . if Afro_corrgovofficials == -1 | Afro_corrgovofficials == 9 | Afro_corrgovofficials == 99
replace Afro_corrgovofficials = ((Afro_corrgovofficials / 3) * -1) + 1

replace Afro_corrpolice = . if Afro_corrpolice == -1 | Afro_corrpolice == 9 | Afro_corrpolice == 99 | Afro_corrpolice == 4
replace Afro_corrpolice = ((Afro_corrpolice / 3) * -1) + 1

replace Afro_corrtaxofficials = . if Afro_corrtaxofficials == -1 | Afro_corrtaxofficials == 9 | Afro_corrtaxofficials == 99 | Afro_corrtaxofficials == 4
replace Afro_corrtaxofficials = ((Afro_corrtaxofficials / 3) * -1) + 1

replace Afro_corrjudmag = . if Afro_corrjudmag == -1 | Afro_corrjudmag == 9 | Afro_corrjudmag == 99 | Afro_corrjudmag == 4
replace Afro_corrjudmag = ((Afro_corrjudmag / 3) * -1) + 1

replace Afro_bribedocper = . if Afro_bribedocper == -1 | Afro_bribedocper == 9 | Afro_bribedocper == 7
replace Afro_bribedocper = ((Afro_bribedocper / 3) * -1) + 1

replace Afro_bribewatersan = . if Afro_bribewatersan == -1 | Afro_bribewatersan == 9 | Afro_bribewatersan == 7
replace Afro_bribewatersan = ((Afro_bribewatersan / 3) * -1) + 1

replace Afro_bribepolice = . if Afro_bribepolice == -1 | Afro_bribepolice == 9 | Afro_bribepolice == 7
replace Afro_bribepolice = ((Afro_bribepolice / 3) * -1) + 1

replace Afro_bribehealth = . if Afro_bribehealth == -1 | Afro_bribehealth == 9 | Afro_bribehealth == 7
replace Afro_bribehealth = ((Afro_bribehealth / 3) * -1) + 1

replace Afro_bribeschool = . if Afro_bribeschool == -1 | Afro_bribeschool == 9 | Afro_bribeschool == 7
replace Afro_bribeschool = ((Afro_bribeschool / 3) * -1) + 1

replace Afro_bribeelection = . if Afro_bribeelection == -1 | Afro_bribeelection == 9 | Afro_bribeelection == 7
replace Afro_bribeelection = ((Afro_bribeelection / 3) * -1) + 1

replace Afro_bribehouseserv = . if Afro_bribehouseserv == -1 | Afro_bribehouseserv == 9 | Afro_bribehouseserv == 7
replace Afro_bribehouseserv = ((Afro_bribehouseserv / 3) * -1) + 1

replace Afro_bribevoters = . if Afro_bribevoters == -1 | Afro_bribevoters == 9 | Afro_bribevoters == 7
replace Afro_bribevoters = ((Afro_bribevoters / 3) * -1) + 1

replace Afro_bribeschoolserv = . if Afro_bribeschoolserv == -1 | Afro_bribeschoolserv == 9 | Afro_bribeschoolserv == 7
replace Afro_bribeschoolserv = ((Afro_bribeschoolserv / 3) * -1) + 1

replace LAPOP_exc7 = . if LAPOP_exc7 != 1 & LAPOP_exc7 != 2 & LAPOP_exc7 != 3 & LAPOP_exc7 != 4
replace LAPOP_exc7 = LAPOP_exc7 - 1
replace LAPOP_exc7 = (LAPOP_exc7 / 3)

replace LAPOP_exc7new = . if LAPOP_exc7new != 1 & LAPOP_exc7new != 2 & LAPOP_exc7new != 3 & LAPOP_exc7new != 4 & LAPOP_exc7new != 5
replace LAPOP_exc7new = LAPOP_exc7new - 1
replace LAPOP_exc7new = ((LAPOP_exc7new / 4) * -1) + 1

replace LAPOP_exc2 = . if LAPOP_exc2 != 1 & LAPOP_exc2 != 0
rename LAPOP_exc2 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc2)
drop a

replace LAPOP_exc6 = . if LAPOP_exc6 != 1 & LAPOP_exc6 != 0
rename LAPOP_exc6 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc6)
drop a

replace LAPOP_exc11 = . if LAPOP_exc11 != 1 & LAPOP_exc11 != 0
rename LAPOP_exc11 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc11)
drop a

replace LAPOP_exc14 = . if LAPOP_exc14 != 1 & LAPOP_exc14 != 0
rename LAPOP_exc14 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc14)
drop a

replace LAPOP_exc15 = . if LAPOP_exc15 != 1 & LAPOP_exc15 != 0
rename LAPOP_exc15 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc15)
drop a

replace LAPOP_exc16 = . if LAPOP_exc16 != 1 & LAPOP_exc16 != 0
rename LAPOP_exc16 a
recode a (0 = 1 "No") ///
		 (1 = 0 "Yes") ///
		 , generate(LAPOP_exc16)
drop a

replace Latin_severitycorr = . if Latin_severitycorr == 99
replace Latin_severitycorr = Latin_severitycorr - 1
replace Latin_severitycorr = (Latin_severitycorr / 3)

replace Latin_percorr = Latin_percorr - 1
replace Latin_percorr = ((Latin_percorr / 9) * -1) + 1

replace Latin_corrpolice = . if Latin_corrpolice == 99
replace Latin_corrpolice = Latin_corrpolice - 1
replace Latin_corrpolice = ((Latin_corrpolice) * -1) + 1 if year == 2010
replace Latin_corrpolice = ((Latin_corrpolice / 3) * -1) + 1 if year != 2010

replace Latin_corrlocalgov = . if Latin_corrlocalgov == 99
replace Latin_corrlocalgov = Latin_corrlocalgov - 1
replace Latin_corrlocalgov = ((Latin_corrlocalgov / 3) * -1) + 1

replace Latin_corrnatgov = . if Latin_corrnatgov == 99
replace Latin_corrnatgov = Latin_corrnatgov - 1
replace Latin_corrnatgov = ((Latin_corrnatgov / 3) * -1) + 1

replace Latin_corrpresoffice = . if Latin_corrpresoffice == 99
replace Latin_corrpresoffice = Latin_corrpresoffice - 1
replace Latin_corrpresoffice = ((Latin_corrpresoffice / 3) * -1) + 1

replace Latin_corrparliament = . if Latin_corrparliament == 99
replace Latin_corrparliament = Latin_corrparliament - 1
replace Latin_corrparliament = ((Latin_corrparliament / 3) * -1) + 1

replace Latin_corrgovoff = . if Latin_corrgovoff == 99
replace Latin_corrgovoff = Latin_corrgovoff - 1
replace Latin_corrgovoff = ((Latin_corrgovoff / 3) * -1) + 1

replace Latin_corrgovcouncil = . if Latin_corrgovcouncil == 99
replace Latin_corrgovcouncil = Latin_corrgovcouncil - 1
replace Latin_corrgovcouncil = ((Latin_corrgovcouncil / 3) * -1) + 1

replace Latin_corrtaxofficials = . if Latin_corrtaxofficials == 99
replace Latin_corrtaxofficials = Latin_corrtaxofficials - 1
replace Latin_corrtaxofficials = ((Latin_corrtaxofficials / 3) * -1) + 1

replace Latin_corrjudge = . if Latin_corrjudge == 99
replace Latin_corrjudge = Latin_corrjudge - 1
replace Latin_corrjudge = ((Latin_corrjudge / 3) * -1) + 1

replace Latin_corrgovnat = . if Latin_corrgovnat == 99
replace Latin_corrgovnat = ((Latin_corrgovnat / 10) * -1) + 1

replace Latin_corrcongress = . if Latin_corrcongress == 99
replace Latin_corrcongress = ((Latin_corrcongress / 10) * -1) + 1

replace Latin_corrgovloc = . if Latin_corrgovloc == 99
replace Latin_corrgovloc = ((Latin_corrgovloc / 10) * -1) + 1

replace Latin_corrcourt = . if Latin_corrcourt == 99
replace Latin_corrcourt = ((Latin_corrcourt / 10) * -1) + 1

replace Latin_corrpresofficeyesno = . if Latin_corrpresofficeyesno == 99
replace Latin_corrpresofficeyesno = (Latin_corrpresofficeyesno * -1) + 2

replace Arab_corrstateyesno = Arab_corrstate if year == 2011
replace Arab_corrstate = . if year == 2011
replace Arab_corrstate = . if Arab_corrstate == 99
replace Arab_corrstate = Arab_corrstate - 1
replace Arab_corrstate = Arab_corrstate / 3

replace Arab_corrstateyesno = . if Arab_corrstateyesno == 99
replace Arab_corrstateyesno = Arab_corrstateyesno - 1

replace WVS_v213 = WVS_v213 - 1
replace WVS_v213 = ((WVS_v213 / 3) * -1) + 1

replace WVS_mn_228n = . if WVS_mn_228n < 1
replace WVS_mn_228n = WVS_mn_228n - 1
replace WVS_mn_228n = ((WVS_mn_228n / 9) * -1) + 1

replace WVS_v228d = WVS_v228d - 1
replace WVS_v228d = WVS_v228d / 3

replace WVS_q112 = WVS_q112 - 1
replace WVS_q112 = ((WVS_q112 / 9) * -1) + 1


replace WVS_q113 = WVS_q113 - 1
replace WVS_q113 = ((WVS_q113 / 3) * -1) + 1

replace WVS_q115 = WVS_q115 - 1
replace WVS_q115 = ((WVS_q115 / 3) * -1) + 1

replace WVS_q116 = WVS_q116 - 1
replace WVS_q116 = ((WVS_q116 / 3) * -1) + 1

replace WVS_q118 = WVS_q118 - 1
replace WVS_q118 = ((WVS_q118 / 3) * -1) + 1

replace WVS_q227 = WVS_q227 - 1
replace WVS_q227 = (WVS_q227 / 3)

replace AFAfghan_x23c = . if AFAfghan_x23c == 998 | AFAfghan_x23c == 999
replace AFAfghan_x23c = 0 if AFAfghan_x23c == 101
replace AFAfghan_x23c = 0.5 if AFAfghan_x23c == 102
replace AFAfghan_x23c = 1 if AFAfghan_x23c == 103

replace AFAfghan_x23e = . if AFAfghan_x23e == 998 | AFAfghan_x23e == 999
replace AFAfghan_x23e = 0 if AFAfghan_x23e == 101
replace AFAfghan_x23e = 0.5 if AFAfghan_x23e == 102
replace AFAfghan_x23e = 1 if AFAfghan_x23e == 103

replace AFTimor_m46_3 = . if AFTimor_m46_3 == 95 | AFTimor_m46_3 == 99
replace AFTimor_m46_3 = AFTimor_m46_3 - 1
replace AFTimor_m46_3 = ((AFTimor_m46_3 / 4) * -1) + 1

replace AFTimor_m47_01 = . if AFTimor_m47_01 == 95 | AFTimor_m47_01 == 99
replace AFTimor_m47_01 = AFTimor_m47_01 - 1
replace AFTimor_m47_01 = ((AFTimor_m47_01 / 4) * -1) + 1

replace AFTimor_m47_03 = . if AFTimor_m47_03 == 95 | AFTimor_m47_03 == 99
replace AFTimor_m47_03 = AFTimor_m47_03 - 1
replace AFTimor_m47_03 = ((AFTimor_m47_03 / 4) * -1) + 1

replace AFTimor_m47_04 = . if AFTimor_m47_04 == 95 | AFTimor_m47_04 == 99
replace AFTimor_m47_04 = AFTimor_m47_04 - 1
replace AFTimor_m47_04 = ((AFTimor_m47_04 / 4) * -1) + 1

replace AFTimor_m47_05 = . if AFTimor_m47_05 == 95 | AFTimor_m47_05 == 99
replace AFTimor_m47_05 = AFTimor_m47_05 - 1
replace AFTimor_m47_05 = ((AFTimor_m47_05 / 4) * -1) + 1

replace AFTimor_m47_07 = . if AFTimor_m47_07 == 95 | AFTimor_m47_07 == 99
replace AFTimor_m47_07 = AFTimor_m47_07 - 1
replace AFTimor_m47_07 = ((AFTimor_m47_07 / 4) * -1) + 1

replace AFTimor_m47_09 = . if AFTimor_m47_09 == 95 | AFTimor_m47_09 == 99
replace AFTimor_m47_09 = AFTimor_m47_09 - 1
replace AFTimor_m47_09 = ((AFTimor_m47_09 / 4) * -1) + 1

replace AFTimor_m47_10 = . if AFTimor_m47_10 == 95 | AFTimor_m47_10 == 99
replace AFTimor_m47_10 = AFTimor_m47_10 - 1
replace AFTimor_m47_10 = ((AFTimor_m47_10 / 4) * -1) + 1

replace AFTimor_m47_11 = . if AFTimor_m47_11 == 95 | AFTimor_m47_11 == 99
replace AFTimor_m47_11 = AFTimor_m47_11 - 1
replace AFTimor_m47_11 = ((AFTimor_m47_11 / 4) * -1) + 1

replace AFTimor_m47_18 = . if AFTimor_m47_18 == 95 | AFTimor_m47_18 == 99
replace AFTimor_m47_18 = AFTimor_m47_18 - 1
replace AFTimor_m47_18 = ((AFTimor_m47_18 / 4) * -1) + 1

replace AFTimor_wave4_q17_h = . if AFTimor_wave4_q17_h == 6 | AFTimor_wave4_q17_h == 7
replace AFTimor_wave4_q17_h = AFTimor_wave4_q17_h - 1
replace AFTimor_wave4_q17_h = AFTimor_wave4_q17_h / 4

replace MAsia_q116 = . if MAsia_q116 == 8 | MAsia_q116 == 9
replace MAsia_q116 = MAsia_q116 - 1
replace MAsia_q116 = ((MAsia_q116 / 3) * -1) + 1

replace MAsia_q117 = . if MAsia_q117 == 8 | MAsia_q117 == 9
replace MAsia_q117 = MAsia_q117 - 1
replace MAsia_q117 = ((MAsia_q117 / 3) * -1) + 1

replace Afro7_Q44A = . if Afro7_Q44A == 8 | Afro7_Q44A == 9
replace Afro7_Q44A = ((Afro7_Q44A / 3) * -1) + 1

replace Afro7_Q44B = . if Afro7_Q44B == 8 | Afro7_Q44B == 9
replace Afro7_Q44B = ((Afro7_Q44B / 3) * -1) + 1

replace Afro7_Q44C = . if Afro7_Q44C == 8 | Afro7_Q44C == 9 | Afro7_Q44C == -1
replace Afro7_Q44C = ((Afro7_Q44C / 3) * -1) + 1

replace Afro7_Q44D = . if Afro7_Q44D == 8 | Afro7_Q44D == 9 | Afro7_Q44D == -1
replace Afro7_Q44D = ((Afro7_Q44D / 3) * -1) + 1

replace Afro7_Q44E = . if Afro7_Q44E == 8 | Afro7_Q44E == 9 | Afro7_Q44E == -1
replace Afro7_Q44E = ((Afro7_Q44E / 3) * -1) + 1

replace Afro7_Q44F = . if Afro7_Q44F == 8 | Afro7_Q44F == 9 | Afro7_Q44F == -1
replace Afro7_Q44F = ((Afro7_Q44F / 3) * -1) + 1

replace Afro7_Q49C = . if Afro7_Q49C == 8 | Afro7_Q49C == 9 | Afro7_Q49C == -1 | Afro7_Q49C == 7
replace Afro7_Q49C = ((Afro7_Q49C / 3) * -1) + 1

replace Afro7_Q49G = . if Afro7_Q49G == 8 | Afro7_Q49G == 9 | Afro7_Q49G == -1 | Afro7_Q49G == 7
replace Afro7_Q49G = ((Afro7_Q49G / 3) * -1) + 1

replace Afro7_Q49K = . if Afro7_Q49K == 8 | Afro7_Q49K == 9 | Afro7_Q49K == -1 | Afro7_Q49K == 7
replace Afro7_Q49K = ((Afro7_Q49K / 3) * -1) + 1

replace Afro8_Q42A = . if Afro8_Q42A == 8 | Afro8_Q42A == 9 | Afro8_Q42A == -1 | Afro8_Q42A == 7
replace Afro8_Q42A = ((Afro8_Q42A / 3) * -1) + 1

replace Afro8_Q42B = . if Afro8_Q42B == 8 | Afro8_Q42B == 9 | Afro8_Q42B == -1 | Afro8_Q42B == 7
replace Afro8_Q42B = ((Afro8_Q42B / 3) * -1) + 1

replace Afro8_Q42C = . if Afro8_Q42C == 8 | Afro8_Q42C == 9 | Afro8_Q42C == -1 | Afro8_Q42C == 7
replace Afro8_Q42C = ((Afro8_Q42C / 3) * -1) + 1

replace Afro8_Q42D = . if Afro8_Q42D == 8 | Afro8_Q42D == 9 | Afro8_Q42D == -1 | Afro8_Q42D == 7 | Afro8_Q42D == 94
replace Afro8_Q42D = ((Afro8_Q42D / 3) * -1) + 1

replace Afro8_Q42E = . if Afro8_Q42E == 8 | Afro8_Q42E == 9 | Afro8_Q42E == -1 | Afro8_Q42E == 7
replace Afro8_Q42E = ((Afro8_Q42E / 3) * -1) + 1

replace Afro8_Q42F = . if Afro8_Q42F == 8 | Afro8_Q42F == 9 | Afro8_Q42F == -1 | Afro8_Q42F == 7
replace Afro8_Q42F = ((Afro8_Q42F / 3) * -1) + 1

replace Afro8_Q42G = . if Afro8_Q42G == 8 | Afro8_Q42G == 9 | Afro8_Q42G == -1 | Afro8_Q42G == 7
replace Afro8_Q42G = ((Afro8_Q42G / 3) * -1) + 1

replace Afro8_Q44C = . if Afro8_Q44C == 8 | Afro8_Q44C == 9 | Afro8_Q44C == -1 | Afro8_Q44C == 7
replace Afro8_Q44C = ((Afro8_Q44C / 3) * -1) + 1

replace Afro8_Q44F = . if Afro8_Q44F == 8 | Afro8_Q44F == 9 | Afro8_Q44F == -1 | Afro8_Q44F == 7
replace Afro8_Q44F = ((Afro8_Q44F / 3) * -1) + 1

replace Afro8_Q44I = . if Afro8_Q44I == 8 | Afro8_Q44I == 9 | Afro8_Q44I == -1 | Afro8_Q44I == 7
replace Afro8_Q44I = ((Afro8_Q44I / 3) * -1) + 1

replace Afro8_Q44N = . if Afro8_Q44N == 8 | Afro8_Q44N == 9 | Afro8_Q44N == -1 | Afro8_Q44N == 7
replace Afro8_Q44N = ((Afro8_Q44N / 3) * -1) + 1

replace Asia_q117 = Asia_q117 - 1
replace Asia_q117 = Asia_q117 / 3

replace Asia_q118 = Asia_q118 - 1
replace Asia_q118 = Asia_q118 / 3

replace WBPOS_povcorreduc = . if WBPOS_povcorreduc < 0

replace TIDRC_CORRPEOPLE1FIN = . if TIDRC_CORRPEOPLE1FIN == 5
replace TIDRC_CORRPEOPLE1FIN = TIDRC_CORRPEOPLE1FIN - 1
replace TIDRC_CORRPEOPLE1FIN = ((TIDRC_CORRPEOPLE1FIN / 3) * -1) + 1

replace TIDRC_CORRPEOPLE2FIN = . if TIDRC_CORRPEOPLE2FIN == 5
replace TIDRC_CORRPEOPLE2FIN = TIDRC_CORRPEOPLE2FIN - 1
replace TIDRC_CORRPEOPLE2FIN = ((TIDRC_CORRPEOPLE2FIN / 3) * -1) + 1

replace TIDRC_CORRPEOPLE3FIN = . if TIDRC_CORRPEOPLE3FIN == 5
replace TIDRC_CORRPEOPLE3FIN = TIDRC_CORRPEOPLE3FIN - 1
replace TIDRC_CORRPEOPLE3FIN = ((TIDRC_CORRPEOPLE3FIN / 3) * -1) + 1

replace TIDRC_CORRPEOPLE4FIN = . if TIDRC_CORRPEOPLE4FIN == 5
replace TIDRC_CORRPEOPLE4FIN = TIDRC_CORRPEOPLE4FIN - 1
replace TIDRC_CORRPEOPLE4FIN = ((TIDRC_CORRPEOPLE4FIN / 3) * -1) + 1

replace TIDRC_CORRPEOPLE5FIN = . if TIDRC_CORRPEOPLE5FIN == 5
replace TIDRC_CORRPEOPLE5FIN = TIDRC_CORRPEOPLE5FIN - 1
replace TIDRC_CORRPEOPLE5FIN = ((TIDRC_CORRPEOPLE5FIN / 3) * -1) + 1

replace TIDRC_CORRPEOPLE7FIN = . if TIDRC_CORRPEOPLE7FIN == 5
replace TIDRC_CORRPEOPLE7FIN = TIDRC_CORRPEOPLE7FIN - 1
replace TIDRC_CORRPEOPLE7FIN = ((TIDRC_CORRPEOPLE7FIN / 3) * -1) + 1

replace TIDRC_BRIBE1FIN = . if TIDRC_BRIBE1FIN == 5 | TIDRC_BRIBE1FIN == 6 | TIDRC_BRIBE1FIN == 7
replace TIDRC_BRIBE1FIN = TIDRC_BRIBE1FIN - 1
replace TIDRC_BRIBE1FIN = ((TIDRC_BRIBE1FIN / 3) * -1) + 1

replace TIDRC_BRIBE2FIN = . if TIDRC_BRIBE2FIN == 5 | TIDRC_BRIBE2FIN == 6 | TIDRC_BRIBE2FIN == 7
replace TIDRC_BRIBE2FIN = TIDRC_BRIBE2FIN - 1
replace TIDRC_BRIBE2FIN = ((TIDRC_BRIBE2FIN / 3) * -1) + 1

replace TIDRC_BRIBE3FIN = . if TIDRC_BRIBE3FIN == 5 | TIDRC_BRIBE3FIN == 6 | TIDRC_BRIBE3FIN == 7
replace TIDRC_BRIBE3FIN = TIDRC_BRIBE3FIN - 1
replace TIDRC_BRIBE3FIN = ((TIDRC_BRIBE3FIN / 3) * -1) + 1

replace TIDRC_BRIBE4FIN = . if TIDRC_BRIBE4FIN == 5 | TIDRC_BRIBE4FIN == 6 | TIDRC_BRIBE4FIN == 7
replace TIDRC_BRIBE4FIN = TIDRC_BRIBE4FIN - 1
replace TIDRC_BRIBE4FIN = ((TIDRC_BRIBE4FIN / 3) * -1) + 1

replace TIDRC_BRIBE5FIN = . if TIDRC_BRIBE5FIN == 5 | TIDRC_BRIBE5FIN == 6 | TIDRC_BRIBE5FIN == 7
replace TIDRC_BRIBE5FIN = TIDRC_BRIBE5FIN - 1
replace TIDRC_BRIBE5FIN = ((TIDRC_BRIBE5FIN / 3) * -1) + 1

replace TI_Q2a = . if TI_Q2a == 99
replace TI_Q2a = TI_Q2a - 1
replace TI_Q2a = ((TI_Q2a / 3) * -1) + 1

replace TI_Q6_1 = . if TI_Q6_1 == 5
replace TI_Q6_1 = TI_Q6_1 - 1
replace TI_Q6_1 = ((TI_Q6_1 / 3) * -1) + 1

replace TI_Q6_2 = . if TI_Q6_2 == 5
replace TI_Q6_2 = TI_Q6_2 - 1
replace TI_Q6_2 = ((TI_Q6_2 / 3) * -1) + 1

replace TI_Q6_3 = . if TI_Q6_3 == 5
replace TI_Q6_3 = TI_Q6_3 - 1
replace TI_Q6_3 = ((TI_Q6_3 / 3) * -1) + 1

replace TI_Q6_4 = . if TI_Q6_4 == 5
replace TI_Q6_4 = TI_Q6_4 - 1
replace TI_Q6_4 = ((TI_Q6_4 / 3) * -1) + 1

replace TI_Q6_5 = . if TI_Q6_5 == 5
replace TI_Q6_5 = TI_Q6_5 - 1
replace TI_Q6_5 = ((TI_Q6_5 / 3) * -1) + 1

replace TI_Q6_6 = . if TI_Q6_6 == 5
replace TI_Q6_6 = TI_Q6_6 - 1
replace TI_Q6_6 = ((TI_Q6_6 / 3) * -1) + 1

replace TI_Q6_7 = . if TI_Q6_7 == 5
replace TI_Q6_7 = TI_Q6_7 - 1
replace TI_Q6_7 = ((TI_Q6_7 / 3) * -1) + 1

replace TI_Q6_10 = . if TI_Q6_10 == 5
replace TI_Q6_10 = TI_Q6_10 - 1
replace TI_Q6_10 = ((TI_Q6_10 / 3) * -1) + 1

replace TI_Q9_1 = . if TI_Q9_1 == 98 | TI_Q9_1 == 99
replace TI_Q9_1 = TI_Q9_1 - 1
replace TI_Q9_1 = ((TI_Q9_1 / 3) * -1) + 1

replace TI_Q9_2 = . if TI_Q9_2 == 98 | TI_Q9_2 == 99
replace TI_Q9_2 = TI_Q9_2 - 1
replace TI_Q9_2 = ((TI_Q9_2 / 3) * -1) + 1

replace TI_Q9_3 = . if TI_Q9_3 == 98 | TI_Q9_3 == 99
replace TI_Q9_3 = TI_Q9_3 - 1
replace TI_Q9_3 = ((TI_Q9_3 / 3) * -1) + 1

replace TI_Q9_4 = . if TI_Q9_4 == 98 | TI_Q9_4 == 99
replace TI_Q9_4 = TI_Q9_4 - 1
replace TI_Q9_4 = ((TI_Q9_4 / 3) * -1) + 1

replace TI_Q9_5 = . if TI_Q9_5 == 98 | TI_Q9_5 == 99
replace TI_Q9_5 = TI_Q9_5 - 1
replace TI_Q9_5 = ((TI_Q9_5 / 3) * -1) + 1

replace TI_Q9_6 = . if TI_Q9_6 == 98 | TI_Q9_6 == 99
replace TI_Q9_6 = TI_Q9_6 - 1
replace TI_Q9_6 = ((TI_Q9_6 / 3) * -1) + 1

replace TI_Q19b = . if TI_Q19b == 97 | TI_Q19b == 98 | TI_Q19b == 99
replace TI_Q19b = TI_Q19b - 1
replace TI_Q19b = ((TI_Q19b / 3) * -1) + 1

replace QoG2013_q15 = . if QoG2013_q15 == 99
replace QoG2013_q15 = ((QoG2013_q15 / 10) * -1) + 1

replace QoG2013_q16_1 = . if QoG2013_q16_1 == 99
replace QoG2013_q16_1 = QoG2013_q16_1 - 1

replace QoG2013_q16_2 = . if QoG2013_q16_2 == 99
replace QoG2013_q16_2 = QoG2013_q16_2 - 1

replace QoG2013_q16_3 = . if QoG2013_q16_3 == 99
replace QoG2013_q16_3 = QoG2013_q16_3 - 1

replace QoG2013_q18 = . if QoG2013_q18 == 99
replace QoG2013_q18 = QoG2013_q18 / 10

replace QoG2017_Q15 = . if QoG2017_Q15 == 99
replace QoG2017_Q15 = QoG2017_Q15 - 1
replace QoG2017_Q15 = ((QoG2017_Q15 / 9) * -1) + 1

replace QoG2017_Q17_2 = . if QoG2017_Q17_2 == 99
replace QoG2017_Q17_2 = QoG2017_Q17_2 - 1

replace QoG2017_Q18_1 = . if QoG2017_Q18_1 == 99
replace QoG2017_Q18_1 = QoG2017_Q18_1 - 1

replace QoG2017_Q18_2 = . if QoG2017_Q18_2 == 99
replace QoG2017_Q18_2 = QoG2017_Q18_2 - 1

replace QoG2017_Q18_3 = . if QoG2017_Q18_3 == 99
replace QoG2017_Q18_3 = QoG2017_Q18_3 - 1

replace QoG2017_Q19 = . if QoG2017_Q19 == 99
replace QoG2017_Q19 = QoG2017_Q19 - 1
replace QoG2017_Q19 = QoG2017_Q19 / 9

replace QoG2021_q16 = . if QoG2021_q16 == 99
replace QoG2021_q16 = QoG2021_q16 - 1
replace QoG2021_q16 = ((QoG2021_q16 / 9) * -1) + 1

replace QoG2021_q18_2 = . if QoG2021_q18_2 == 99
replace QoG2021_q18_2 = QoG2021_q18_2 - 1

replace QoG2021_q19_1 = . if QoG2021_q19_1 == 99
replace QoG2021_q19_1 = QoG2021_q19_1 - 1

replace QoG2021_q19_2 = . if QoG2021_q19_2 == 99
replace QoG2021_q19_2 = QoG2021_q19_2 - 1

replace QoG2021_q19_3 = . if QoG2021_q19_3 == 99
replace QoG2021_q19_3 = QoG2021_q19_3 - 1

order iso scountry wt regname regcode year Source Notes Afro_* Afro7* Afro8* QoG2013* QoG2017* QoG2021* WBPOS* LAPOP* Latin* Arab* WVS* AFAfghan* AFTimor* AFMyan* Asia_q117 Asia_q118 MAsia* WBES* TIDRC* TI_Q*, first
drop syear

// Combine Eurobarometers

replace Euro1_corrprob = Euro2_corrprob if missing(Euro1_corrprob)
replace Euro1_corrlocgov = Euro2_corrlocgov if missing(Euro1_corrlocgov)
replace Euro1_corrnatgov = Euro2_corrnatgov if missing(Euro1_corrnatgov)
replace Euro1_corrpoliceyesno = Euro2_corrpoliceyesno if missing(Euro1_corrpoliceyesno)
replace Euro1_corrjudgeyesno = Euro2_corrjudgeyesno if missing(Euro1_corrjudgeyesno)
replace Euro1_corrpoliticianyesno = Euro2_corrpoliticianyesno if missing(Euro1_corrpoliticianyesno)
replace Euro1_bribepoliceyesno = Euro2_bribepoliceyesno if missing(Euro1_bribepoliceyesno)
replace Euro1_bribejudgeyesno = Euro2_bribejudgeyesno if missing(Euro1_bribejudgeyesno)
replace Euro1_bribehealthyesno = Euro2_bribehealthyesno if missing(Euro1_bribehealthyesno)
replace Euro1_bribeeducyesno = Euro2_bribeeducyesno if missing(Euro1_bribeeducyesno)
replace Euro1_corrgovoffyesno = Euro2_corrgovoffyesno if missing(Euro1_corrgovoffyesno)
replace Euro1_bribegovoffyesno = Euro2_bribegovoffyesno if missing(Euro1_bribegovoffyesno)
rename Euro2_corrtaxyesno Euro1_corrtaxyesno
drop Euro2*
rename Euro1* Euro*

save "Temp\Individual Pre Dimension Pre GCB-Eurobarometer Conversion.dta", replace

// 3. Attach time and subnational variation of Eurobarometers (at individual level) around those of Global Corruption Barometers such that questions become comparable for later dimension-building.
use "Temp\Individual Pre Dimension Pre GCB-Eurobarometer Conversion.dta"

keep iso-Notes Euro*
egen a = rowmiss(Euro_corrprob-Euro_corrtaxyesno)
drop if a == 13
drop a
collapse (mean) Euro_corrprob-Euro_corrtaxyesno (first) scountry [aweight=wt], by(iso year)

merge 1:1 iso year using "National External\GCB-2017_Global-Results.dta"
drop if _merge == 2
keep iso year scountry Euro_corrpoliceyesno Euro_corrjudgeyesno Euro_corrtaxyesno Euro_corrgovoffyesno GCB*
sort scountry year
local factors "govoff judge tax police"
foreach fac of local factors {
	bys scountry: replace GCB_corr`fac' = GCB_corr`fac'[_n+1] - Euro_corr`fac'[_n+1] + Euro_corr`fac' if year == 2011
	bys scountry: replace GCB_corr`fac' = GCB_corr`fac'[_n+1] - Euro_corr`fac'[_n+1] + Euro_corr`fac' if year == 2007
	bys scountry: replace GCB_corr`fac' = GCB_corr`fac'[_n-1] - Euro_corr`fac'[_n-1] + Euro_corr`fac' if year == 2019
	bys scountry: replace GCB_corr`fac' = GCB_corr`fac'[_n-1] - Euro_corr`fac'[_n-1] + Euro_corr`fac' if year == 2022
	bys scountry: egen x = max(GCB_sd`fac')
	replace GCB_sd`fac' = x if missing(GCB_sd`fac') & !missing(GCB_corr`fac')
	drop x
}
merge 1:1 iso year using "National External\GCB-2020_EU-Results.dta"
drop if _merge == 2
drop _merge
sort scountry year
local factors "govoff judge police"
foreach fac of local factors {
	bys scountry: replace GCBEU_corr`fac' = GCBEU_corr`fac'[_n+1] - Euro_corr`fac'[_n+1] + Euro_corr`fac' if year == 2017
	bys scountry: replace GCBEU_corr`fac' = GCBEU_corr`fac'[_n-1] - Euro_corr`fac'[_n-1] + Euro_corr`fac' if year == 2022
	bys scountry: replace GCBEU_corr`fac' = GCBEU_corr`fac'[_n+1] - Euro_corr`fac'[_n+1] + Euro_corr`fac' if year == 2011
	bys scountry: replace GCBEU_corr`fac' = GCBEU_corr`fac'[_n+1] - Euro_corr`fac'[_n+1] + Euro_corr`fac' if year == 2007
	bys scountry: egen x = max(GCBEU_sd`fac')
	replace GCBEU_sd`fac' = x if missing(GCBEU_sd`fac') & !missing(GCBEU_corr`fac')
	drop x
}
levelsof scountry if missing(GCB_corrjudge), local(countries)
local factors "police judge govoff"
foreach c of local countries {
	foreach fac of local factors {
		replace GCB_corr`fac' = GCBEU_corr`fac' if scountry == "`c'"
		replace GCB_sd`fac' = GCBEU_sd`fac' if scountry == "`c'"
	}
}
keep iso year scountry GCB_corrjudge GCB_sdjudge GCB_corrgovoff GCB_sdgovoff GCB_corrpolice GCB_sdpolice GCB_corrtax GCB_sdtax
order iso year scountry
save "Temp\FinalGCBAdjustments_TrendExtrapolation.dta", replace

// 4. Create 19 dimensions by allocating questions to each dimension. Additionally, perform adjustments where necessary to make questions as comparable as possible (harmonization). First, we make adjustments such that the Eurobarometers are attached around the Global Corruption Barometers, and that the WBPOS and WBES are adjusted based on overlap with other surveys. Then we adjust on a per-dimension basis.

use "Temp\Individual Pre Dimension Pre GCB-Eurobarometer Conversion.dta", clear

desc // 185 vars, 177 questions

replace Afro_bribewatersan = Afro_bribehouseserv if missing(Afro_bribewatersan)
drop Afro_bribehouseserv

merge m:1 iso year using "Temp\FinalGCBAdjustments_TrendExtrapolation.dta"
drop _merge

local factors "tax govoff police judge"
local i "2007 2011 2017 2019 2022"
foreach fac of local factors {
	levelsof scountry if !missing(Euro_corr`fac'yesno) & !missing(GCB_corr`fac'), local(countries)
	generate eurocorr`fac'adj = 0
	foreach country in `countries' {
		foreach year of local i {
			display "[Transform] '`country'' from Eurobarometers `year' to GCB 2017 trends, corr`fac'"
			quietly summ Euro_corr`fac'yesno [aweight=wt] if scountry == "`country'" & year == `year'
			if r(N) > 0 {
				replace Euro_corr`fac'yesno = (Euro_corr`fac'yesno - `r(mean)') / `r(sd)' if scountry == "`country'" & year == `year'
				replace Euro_corr`fac'yesno = (Euro_corr`fac'yesno * GCB_sd`fac') + GCB_corr`fac' if scountry == "`country'" & year == `year'
				replace eurocorr`fac'adj = 1 if scountry == "`country'" & year == `year'
				quietly summ Euro_corr`fac'yesno GCB_corr`fac' GCB_sd`fac' [aweight=wt] if scountry == "`country'" & year == `year' & eurocorr`fac'adj == 1
				replace Euro_corr`fac'yesno = . if eurocorr`fac'adj == 0 & year == `year' & scountry == "`country'"
			}
			else {
				display "[Transform] failed in `year' for `country' for corr`fac' - no data."
			}
		}
	}
	replace Euro_corr`fac'yesno = . if eurocorr`fac'adj == 0
	drop eurocorr`fac'adj
}

drop GCB_corrgovoff-GCB_sdjudge

// WBPOS

local WBPOSs "WBPOS_povcorreduc"
foreach WBPOS of local WBPOSs {
	replace WBPOS_corrprob = `WBPOS' if !missing(`WBPOS') & `WBPOS' < WBPOS_corrprob
}
rename WBPOS_corrprob WBPOS_state
drop WBPOS_povcorreduc 
tab scountry if !missing(WBPOS_state)
bys scountry year: egen totwbpos = count(scountry) if !missing(WBPOS_state)
quietly summ WBPOS_state [aweight=wt] if scountry == "Tunisia" | scountry == "Jordan"
local badmean = `r(mean)'
quietly summ WVS_mn_228n [aweight=wt] if scountry == "Tunisia" | scountry == "Jordan"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBPOS_state) & totwbpos != 1, local(years)
foreach i of local years {
	levelsof scountry if !missing(WBPOS_state) & totwbpos != 1 & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBPOS_state [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBPOS_state = (((WBPOS_state - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}
drop if totwbpos == 1
drop totwbpos

// WBES
tostring year, gen(syear)
gen a = scountry + syear

// Grand: State & Institutions WBES questions
quietly summ WBES_corr2 if scountry == "Iraq2011" | scountry == "Jordan2013" | scountry == "Tunisia2013"
local badmean = `r(mean)'
quietly summ WVS_mn_228n [aweight=wt] if scountry == "Tunisia" | scountry == "Jordan" | scountry == "Iraq"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBES_corr2) & !missing(iso), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr2) & !missing(iso) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr2 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr2 = (((WBES_corr2 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}

// Grand: Taxation WBES questions
gen temptax = Afro_corrtaxofficials if a == "Namibia2014" | a == "Nigeria2014" | a == "Senegal2014"
replace temptax = Afro8_Q42G if a == "Tunisia2020"
replace temptax = Latin_corrtaxofficials if a == "Dominican Republic2016" | a == "El Salvador2016" | a == "Honduras2016" | a == "Nicaragua2016"
quietly summ WBES_corr1 if scountry == "Namibia2014" | scountry == "Nigeria2014" | scountry == "Senegal2014" | scountry == "Tunisia2020" | scountry == "Dominican Republic2016" | scountry == "El Salvador2016" | scountry == "Honduras2016" | scountry == "Nicaragua2016"
local badmean = `r(mean)'
quietly summ temptax [aweight=wt]
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBES_corr1) & !missing(iso), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr1) & !missing(iso) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr1 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr1 = (((WBES_corr1 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}
drop temptax

// Petty: Government Officials WBES questions
gen z = WVS_q118
replace z = 0 if WVS_q118 != 1 & !missing(WVS_q118)

quietly summ WBES_corr4 if scountry == "Argentina2017" | scountry == "Bolivia2017" | scountry == "Cyprus2019"
local badmean = `r(mean)'
quietly summ z [aweight=wt] if a == "Argentina2017" | a == "Bolivia2017" | a == "Cyprus2019"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBES_corr4) & !missing(iso), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr4) & !missing(iso) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr4 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr4 = (((WBES_corr4 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}
drop z

// Petty: Utilities WBES questions
replace WBES_corr6 = 0 if (WBES_corr7 == 0 | WBES_corr5 == 0)
drop WBES_corr7 WBES_corr5
quietly summ WBES_corr6 if scountry == "Namibia2014" | scountry == "Nigeria2014" | scountry == "Senegal2014"
local badmean = `r(mean)'
quietly summ Afro_bribewatersan [aweight=wt] if a == "Namibia2014" | a == "Nigeria2014" | a == "Senegal2014"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBES_corr6) & !missing(iso), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr6) & !missing(iso) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr6 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr6 = (((WBES_corr6 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}

// Petty: Document WBES questions
replace WBES_corr8 = 0 if WBES_corr9 == 0 | WBES_corr10 == 0
drop WBES_corr9 WBES_corr10
quietly summ WBES_corr8 if scountry == "Dominican Republic2010" | scountry == "Dominican Republic2016" | scountry == "Costarica2010" | scountry == "Colombia2006" | scountry == "Colombia2010" | scountry == "Bolivia2006" | scountry == "Belize2010" | scountry == "Uruguay2010" | scountry == "Peru2006" | scountry == "Peru2010" | scountry == "Peru2017" | scountry == "Paraguay2006" | scountry == "Paraguay2010" | scountry == "Panama2006" | scountry == "Panama2010" | scountry == "Nicaragua2006" | scountry == "Nicaragua2010" | scountry == "Nicaragua2016" | scountry == "Mexico2006" | scountry == "Mexico2010" | scountry == "Jamaica2010" | scountry == "Honduras2006" | scountry == "Honduras2010" | scountry == "Honduras2016" | scountry == "Haiti2006" | scountry == "Haiti2010" | scountry == "Guyana2010" | scountry == "Guatemala2006" | scountry == "Guatemala2010" | scountry == "Guatemala2017" | scountry == "El Salvador2006" | scountry == "El Salvador2010" | scountry == "El Salvador2016" | scountry == "Ecuador2006" | scountry == "Ecuador2010"
local badmean = `r(mean)'
quietly summ LAPOP_exc11 [aweight=wt] if a == "Dominican Republic2010" | a == "Dominican Republic2016" | a == "Costa Rica2010" | a == "Colombia2006" | a == "Colombia2010" | a == "Bolivia2006" | a == "Belize2010" | a == "Uruguay2010" | a == "Peru2006" | a == "Peru2010" | a == "Peru2017" | a == "Paraguay2006" | a == "Paraguay2010" | a == "Panama2006" | a == "Panama2010" | a == "Nicaragua2006" | a == "Nicaragua2010" | a == "Nicaragua2016" | a == "Mexico2006" | a == "Mexico2010" | a == "Jamaica2010" | a == "Honduras2006" | a == "Honduras2010" | a == "Honduras2016" | a == "Haiti2006" | a == "Haiti2010" | a == "Guyana2010" | a == "Guatemala2006" | a == "Guatemala2010" | a == "Guatemala2017" | a == "El Salvador2006" | a == "El Salvador2010" | a == "El Salvador2016" | a == "Ecuador2006" | a == "Ecuador2010"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WBES_corr8) & !missing(iso), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr8) & !missing(iso) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr8 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr8 = (((WBES_corr8 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}

drop if missing(iso)

save "Temp\Masterfile Individual Pre Dimension Pre Impute.dta", replace

// Start Dimension Building

use "Temp\Masterfile Individual Pre Dimension Pre Impute.dta", clear

// Grand: Executive
replace TI_Q6_1 = TI_Q6_2 if TI_Q6_2 < TI_Q6_1 & !missing(TI_Q6_2)
replace TI_Q6_1 = TI_Q6_2 if missing(TI_Q6_1)
drop TI_Q6_2
replace AFTimor_m47_09 = AFTimor_m47_10 if AFTimor_m47_10 < AFTimor_m47_09 & !missing(AFTimor_m47_10)
replace AFTimor_m47_09 = AFTimor_m47_10 if missing(AFTimor_m47_09)
drop AFTimor_m47_10
drop Latin_corrpresofficeyesno
generate corrpres = .
local corrpress "Afro_corrpresoff Afro7_Q44A Afro8_Q42A Latin_corrpresoffice AFTimor_m47_09 TI_Q6_1 TIDRC_CORRPEOPLE1FIN "
foreach corrpres of local corrpress {
	replace corrpres = `corrpres' if !missing(`corrpres')
	drop `corrpres'
}

// Grand: Legislative

quietly summ Latin_corrparliament [aweight=wt] if scountry == "Argentina" | scountry == "Bolivia" | scountry == "Chile" | scountry == "Colombia" | scountry == "Dominican Republic" | scountry == "Ecuador" | scountry == "El Salvador" | scountry == "Guatemala" | scountry == "Honduras" | scountry == "Mexico" | scountry == "Nicaragua" | scountry == "Peru" | scountry == "Uruguay"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
quietly summ Latin_corrcongress [aweight=wt] if scountry == "Argentina" | scountry == "Bolivia" | scountry == "Chile" | scountry == "Colombia" | scountry == "Dominican Republic" | scountry == "Ecuador" | scountry == "El Salvador" | scountry == "Guatemala" | scountry == "Honduras" | scountry == "Mexico" | scountry == "Nicaragua" | scountry == "Peru" | scountry == "Uruguay"
local badmean = `r(mean)'

local newcountries "Brazil"
foreach nc of local newcountries {
	quietly summ Latin_corrcongress [aweight=wt] if scountry == "`nc'"
	replace Latin_corrcongress = (((Latin_corrcongress - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`nc'"
}

levelsof scountry if !missing(Latin_corrparliament) & year == 2018, local(countries)
foreach country of local countries {
	quietly summ Latin_corrparliament [aweight=wt] if scountry == "`country'"
	local goodmean = `r(mean)'
	local goodsd = `r(sd)'
	quietly summ Latin_corrcongress [aweight=wt] if scountry == "`country'"
	replace Latin_corrcongress = (((Latin_corrcongress - `r(mean)') / `r(sd)') * `goodsd') + `goodmean' if scountry == "`country'"
}

generate corrparl = .
local corrparls "Afro_corrparl Afro7_Q44B Afro8_Q42B Latin_corrparliament Latin_corrcongress AFTimor_m47_03 TI_Q6_3 TIDRC_CORRPEOPLE2FIN"
foreach corrparl of local corrparls {
	replace corrparl = `corrparl' if !missing(`corrparl')
	drop `corrparl'
}

// Grand: State & Institutions

gen tempstate = Latin_corrgovnat if year == 2017 & (scountry == "Argentina" | scountry == "Bolivia")
replace tempstate = Euro_corrnatgov if (year == 2017 & scountry == "Greece") | (year == 2019 & scountry == "Cyprus")
quietly summ WVS_q113 [aweight=wt] if (year == 2017 & (scountry == "Argentina" | scountry == "Bolivia" | scountry == "Greece")) | (year == 2019 & scountry == "Cyprus")
local badmean = `r(mean)'
quietly summ tempstate [aweight=wt]
local goodmean = `r(mean)'
local goodsd = `r(sd)'
display "The average adjustment from people to severity in corrstate is: `goodmean' - `badmean'"
generate People2SeverityMu = `goodmean' - `badmean'
generate People2SeveritySigma = `goodsd'
forval i = 2017(1)2020 {
	levelsof scountry if !missing(WVS_q113) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 2019 & "`country'" == "Cyprus") | (`i' == 2017 & ("`country'" == "Argentina" | "`country'" == "Bolivia" | "`country'" == "Greece")) {
			quietly summ tempstate [aweight=wt] if scountry == "`country'" & year == `i' //
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ WVS_q113 [aweight=wt] if scountry == "`country'" & year == `i'
			generate Ppl2SevMu`country' = `goodmeani' - `r(mean)' if scountry == "`country'"
			generate Ppl2SevSigma`country' = `goodsdi' if scountry == "`country'"
			replace WVS_q113 = (((WVS_q113 - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ WVS_q113 [aweight=wt] if scountry == "`country'" & year == `i'
			replace WVS_q113 = (((WVS_q113 - `r(mean)') / `r(sd)') * People2SeveritySigma) + `r(mean)' + People2SeverityMu if scountry == "`country'" & year == `i'
		}
	}
}

levelsof scountry if !missing(Latin_corrnatgov), local(countries)
foreach country of local countries {
	if ("`country'" == "Argentina" | "`country'" == "Bolivia") {
		quietly summ Latin_corrnatgov [aweight=wt] if scountry == "`country'"
		replace Latin_corrnatgov = (((Latin_corrnatgov - `r(mean)') / `r(sd)') * Ppl2SevSigma`country') + `r(mean)' + Ppl2SevMu`country' if scountry == "`country'"
	}
	else {
		quietly summ Latin_corrnatgov [aweight=wt] if scountry == "`country'"
		replace Latin_corrnatgov = (((Latin_corrnatgov - `r(mean)') / `r(sd)') * People2SeveritySigma) + `r(mean)' + People2SeverityMu if scountry == "`country'"
	}
}

quietly summ Asia_q118 [aweight=wt] if (year == 2018 & (scountry == "China" | scountry == "Thailand")) | (year == 2019 & scountry == "Japan")
local badmean = `r(mean)'
quietly summ WVS_q113 [aweight=wt] if (year == 2018 & (scountry == "China" | scountry == "Thailand")) | (year == 2019 & scountry == "Japan")
local goodmean = `r(mean)'
local goodsd = `r(sd)'
local years "2019 2018 2016 2015 2014"
foreach i of local years {
	levelsof scountry if !missing(Asia_q118) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 2019 & "`country'" == "Japan") | (`i' == 2018 & ("`country'" == "China" | "`country'" == "Thailand")) {
			quietly summ WVS_q113 [aweight=wt] if scountry == "`country'" & year == `i'
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ Asia_q118 [aweight=wt] if scountry == "`country'" & year == `i'
			generate Ppl2SevMu`country' = `goodmeani' - `r(mean)' if scountry == "`country'"
			generate Ppl2SevSigma`country' = `goodsdi' if scountry == "`country'"
			replace Asia_q118 = (((Asia_q118 - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else if (`i' == 2016 & "`country'" == "Japan") | (`i' == 2014 & ("`country'" == "Thailand")) {
			quietly summ Asia_q118 [aweight=wt] if scountry == "`country'" & year == `i'
			replace Asia_q118 = (((Asia_q118 - `r(mean)') / `r(sd)') * Ppl2SevSigma`country') + `r(mean)' + Ppl2SevMu`country' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ Asia_q118 [aweight=wt] if scountry == "`country'" & year == `i'
			replace Asia_q118 = (((Asia_q118 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}
quietly summ MAsia_q117 [aweight=wt]
replace MAsia_q117 = (((MAsia_q117 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean'

quietly summ Arab_corrstateyesno [aweight=wt] if (year == 2014 & (scountry == "Jordan" | scountry == "Kuwait" | scountry == "Yemen"))
local badmean = `r(mean)'
quietly summ WVS_mn_228n [aweight=wt] if (year == 2014 & (scountry == "Jordan" | scountry == "Kuwait" | scountry == "Yemen"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
local years "2014 2011"
foreach i of local years {
		levelsof scountry if !missing(Arab_corrstateyesno) & year == `i', local(countries)
		foreach country of local countries {
		if  (`i' == 2014 & ("`country'" == "Kuwait" | "`country'" == "Jordan" | "`country'" == "Yemen")) {
			quietly summ WVS_mn_228n [aweight=wt] if scountry == "`country'" & year == `i'
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ Arab_corrstateyesno [aweight=wt] if scountry == "`country'" & year == `i'
			generate Ppl2SevMu`country' = `goodmeani' - `r(mean)' if scountry == "`country'"
			generate Ppl2SevSigma`country' = `goodsdi' if scountry == "`country'"
			replace Arab_corrstateyesno = (((Arab_corrstateyesno - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else if (`i' == 2011 & ("`country'" == "Jordan" | "`country'" == "Yemen")) {
			quietly summ Arab_corrstateyesno [aweight=wt] if scountry == "`country'" & year == `i'
			replace Arab_corrstateyesno = (((Arab_corrstateyesno - `r(mean)') / `r(sd)') * Ppl2SevSigma`country') + `r(mean)' + Ppl2SevMu`country' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ Arab_corrstateyesno [aweight=wt] if scountry == "`country'" & year == `i'
			replace Arab_corrstateyesno = (((Arab_corrstateyesno - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}	

generate corrstate = .

local corrstates "Arab_corrstate Arab_corrstateyesno WVS_q113 Latin_corrnatgov Latin_corrgovnat WVS_mn_228n AFTimor_m47_11 WBES_corr2 WBPOS_state Asia_q118 MAsia_q117 TI_Q2a Euro_corrnatgov"
foreach corrstate of local corrstates {
	replace corrstate = `corrstate' if !missing(`corrstate')
	drop `corrstate'
}

drop People2* Ppl2* tempstate

// Grand: Judicial

replace AFTimor_m46_3 = AFTimor_m47_07 if AFTimor_m47_07 < AFTimor_m46_3 & !missing(AFTimor_m47_07)
replace AFTimor_m46_3 = AFTimor_m47_07 if missing(AFTimor_m46_3)
drop AFTimor_m47_07

quietly summ Latin_corrjudge [aweight=wt] if scountry == "Argentina" | scountry == "Bolivia" | scountry == "Chile" | scountry == "Colombia" | scountry == "Dominican Republic" | scountry == "Ecuador" | scountry == "El Salvador" | scountry == "Guatemala" | scountry == "Honduras" | scountry == "Mexico" | scountry == "Nicaragua" | scountry == "Peru" | scountry == "Uruguay"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
quietly summ Latin_corrcourt [aweight=wt] if scountry == "Argentina" | scountry == "Bolivia" | scountry == "Chile" | scountry == "Colombia" | scountry == "Dominican Republic" | scountry == "Ecuador" | scountry == "El Salvador" | scountry == "Guatemala" | scountry == "Honduras" | scountry == "Mexico" | scountry == "Nicaragua" | scountry == "Peru" | scountry == "Uruguay"
local badmean = `r(mean)'

local newcountries "Brazil"
foreach nc of local newcountries {
	quietly summ Latin_corrcourt [aweight=wt] if scountry == "`nc'"
	replace Latin_corrcourt = (((Latin_corrcourt - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`nc'"
}

levelsof scountry if !missing(Latin_corrjudge) & year == 2018, local(countries)
foreach country of local countries {
	quietly summ Latin_corrjudge [aweight=wt] if scountry == "`country'"
	local goodmean = `r(mean)'
	local goodsd = `r(sd)'
	quietly summ Latin_corrcourt [aweight=wt] if scountry == "`country'"
	replace Latin_corrcourt = (((Latin_corrcourt - `r(mean)') / `r(sd)') * `goodsd') + `goodmean' if scountry == "`country'"
}

generate corrjudge = .
local corrjudges "Afro_corrjudmag Afro7_Q44F Afro8_Q42F Latin_corrjudge Latin_corrcourt AFTimor_m46_3 TI_Q6_7 TIDRC_CORRPEOPLE7FIN Euro_corrjudgeyesno"
foreach corrjudge of local corrjudges {
	replace corrjudge = `corrjudge' if !missing(`corrjudge')
	drop `corrjudge'
}

// Grand: Taxation
 
generate corrtax = .
local corrtaxs "Afro_corrtaxofficials Afro8_Q42G Latin_corrtaxofficials WBES_corr1 Euro_corrtaxyesno"
foreach corrtax of local corrtaxs {
	replace corrtax = `corrtax' if !missing(`corrtax')
	drop `corrtax'
}

// Grand: Civil
generate corrcivil = .
replace TI_Q6_4 = TI_Q6_10 if missing(TI_Q6_4)
drop TI_Q6_10
local corrcivils "TI_Q6_4 WVS_q116 Afro8_Q42C Latin_percorr"
foreach corrcivil of local corrcivils {
	replace corrcivil = `corrcivil' if !missing(`corrcivil')
	drop `corrcivil'
}


// Grand: Politicians

generate corrpolitician = .
local corrpoliticians "LAPOP_exc7new AFTimor_m47_01 ISSP_v60"
foreach corrpolitician of local corrpoliticians {
	replace corrpolitician = `corrpolitician' if !missing(`corrpolitician')
	drop `corrpolitician'
}

// Grand: Government Officials

quietly summ LAPOP_exc7 [aweight=wt] if (year == 2018 & (scountry == "Colombia" | scountry == "El Salvador" | scountry == "Honduras"))
local badmean = `r(mean)'
quietly summ Latin_corrgovoff [aweight=wt] if (year == 2018 & (scountry == "Colombia" | scountry == "El Salvador" | scountry == "Honduras"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
replace scountry = "ElSalva" if scountry == "El Salvador"
local years "2019 2018 2017 2016 2014 2012 2010 2009 2008 2007 2006 2004"
foreach i of local years {
	levelsof scountry if !missing(LAPOP_exc7) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 2018 & ("`country'" == "Colombia" | "`country'" == "Honduras" | "`country'" == "ElSalvador")) {
			quietly summ Latin_corrgovoff [aweight=wt] if scountry == "`country'" & year == `i'
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ LAPOP_exc7 [aweight=wt] if scountry == "`country'" & year == `i'
			generate Ppl2SevMu`country' = `goodmeani' - `r(mean)' if scountry == "`country'"
			generate Ppl2SevSigma`country' = `goodsdi' if scountry == "`country'"
			replace LAPOP_exc7 = (((LAPOP_exc7 - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else if (`i' != 2018 & ("`country'" == "Colombia" | "`country'" == "Honduras" | "`country'" == "ElSalvador")) {
			quietly summ LAPOP_exc7 [aweight=wt] if scountry == "`country'" & year == `i'
			replace LAPOP_exc7 = (((LAPOP_exc7 - `r(mean)') / `r(sd)') * Ppl2SevSigma`country') + `r(mean)' + Ppl2SevMu`country' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ LAPOP_exc7 [aweight=wt] if scountry == "`country'" & year == `i'
			replace LAPOP_exc7 = (((LAPOP_exc7 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}
replace scountry = "El Salvador" if scountry == "ElSalva"

generate corrgovoff = .
local corrgovoffs "Afro_corrgovofficials Afro7_Q44C LAPOP_exc7 Latin_corrgovoff TIDRC_CORRPEOPLE3FIN ISSP_v61 Euro_corrgovoffyesno"
foreach corrgovoff of local corrgovoffs {
	replace corrgovoff = `corrgovoff' if !missing(`corrgovoff')
	drop `corrgovoff'
}

drop Ppl2*

// Grand: Severity

quietly summ WVS_v213 [aweight=wt] if (year == 1996 & scountry == "Peru") | (year == 1999 & scountry == "El Salvador")
local badmean = `r(mean)'
quietly summ Latin_severitycorr [aweight=wt] if (year == 1998 & (scountry == "Peru" | scountry == "El Salvador"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
display "The average adjustment from people to severity in corrstate is: `goodmean' - `badmean'"
levelsof year if !missing(WVS_v213), local(years)
foreach i of local years {
	levelsof scountry if !missing(WVS_v213) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 1996 & "`country'" == "Peru") | (`i' == 1999 & "`country'" == "El Salvador") {
			quietly summ Latin_severitycorr [aweight=wt] if scountry == "`country'" & year == 1998
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ WVS_v213 [aweight=wt] if scountry == "`country'" & year == `i'
			replace WVS_v213 = (((WVS_v213 - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ WVS_v213 [aweight=wt] if scountry == "`country'" & year == `i'
			replace WVS_v213 = (((WVS_v213 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

generate corrseverity = .
local corrseveritys "Latin_severitycorr WVS_q112 AFTimor_wave4_q17_h WVS_v213 AFAfghan_x23e"
foreach corrseverity of local corrseveritys {
	replace corrseverity = `corrseverity' if !missing(`corrseverity')
	drop `corrseverity'
}

// Grand: Authorities
replace Latin_corrpolice = . if year == 2010

generate QoG_corrpolice = QoG2013_q15
replace QoG_corrpolice = QoG2017_Q15 if missing(QoG_corrpolice)
replace QoG_corrpolice = QoG2021_q16 if missing(QoG_corrpolice)
drop QoG2013_q15 QoG2017_Q15 QoG2021_q16
quietly summ QoG_corrpolice [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local badmean = `r(mean)'
quietly summ Euro_corrpolice [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(QoG_corrpolice), local(years)
foreach i of local years {
	levelsof scountry if !missing(QoG_corrpolice) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Belgium" | "`country'" == "Bulgaria" | "`country'" == "Czech Republic" | "`country'" == "Denmark" | "`country'" == "Finland" | "`country'" == "France" | "`country'" == "Germany" | "`country'" == "Greece" | "`country'" == "Hungary" | "`country'" == "Italy" | "`country'" == "Netherlands" | "`country'" == "Poland" | "`country'" == "Portugal" | "`country'" == "Romania" | "`country'" == "Slovakia" | "`country'" == "Spain" | "`country'" == "Sweden") {
			quietly summ Euro_corrpolice [aweight=wt] if scountry == "`country'" & year == 2017
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ QoG_corrpolice [aweight=wt] if scountry == "`country'" & year == 2017
			local badmeani = `r(mean)'
			quietly summ QoG_corrpolice [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_corrpolice = (((QoG_corrpolice - `r(mean)') / `r(sd)') * `goodsdi') + `r(mean)' + `goodmeani' - `badmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ QoG_corrpolice [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_corrpolice = (((QoG_corrpolice - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

generate corrpolice = .
local corrpolices "Afro_corrpolice Afro7_Q44E Afro8_Q42E Latin_corrpolice AFTimor_m47_05 TI_Q6_6 QoG_corrpolice TIDRC_CORRPEOPLE5FIN Euro_corrpoliceyesno"
foreach corrpolice of local corrpolices {
	replace corrpolice = `corrpolice' if !missing(`corrpolice')
	drop `corrpolice'
}

// Grand: Local Governance
replace AFTimor_m47_04 = AFTimor_m47_18 if AFTimor_m47_18 < AFTimor_m47_04 & !missing(AFTimor_m47_18)
replace AFTimor_m47_04 = AFTimor_m47_18 if missing(AFTimor_m47_04)
drop AFTimor_m47_18

quietly summ Latin_corrgovloc [aweight=wt] if year == 2017 & (scountry == "Argentina" | scountry == "Bolivia")
local badmean = `r(mean)'
quietly summ WVS_q115 [aweight=wt] if year == 2017 & (scountry == "Argentina" | scountry == "Bolivia")
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(Latin_corrgovloc), local(years)
foreach i of local years {
	levelsof scountry if !missing(Latin_corrgovloc) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Argentina" | "`country'" == "Bolivia") {
			quietly summ WVS_q115 [aweight=wt] if scountry == "`country'" & year == 2017
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ Latin_corrgovloc [aweight=wt] if scountry == "`country'" & year == `i'
			replace Latin_corrgovloc = (((Latin_corrgovloc - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ Latin_corrgovloc [aweight=wt] if scountry == "`country'" & year == `i'
			replace Latin_corrgovloc = (((Latin_corrgovloc - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

levelsof year if !missing(AFTimor_m47_04), local(years)
foreach i of local years {
	quietly summ AFTimor_m47_04 [aweight=wt] if year == `i'
	replace AFTimor_m47_04 = (((AFTimor_m47_04 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if year == `i'
}
levelsof year if !missing(AFAfghan_x23c), local(years)
foreach i of local years {
	quietly summ AFAfghan_x23c [aweight=wt] if year == `i'
	replace AFAfghan_x23c = (((AFAfghan_x23c - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if year == `i'
}
quietly summ Euro_corrlocgov [aweight=wt] if (year == 2017 & scountry == "Greece") | (year == 2019 & scountry == "Cyprus")
local badmean = `r(mean)'
quietly summ WVS_q115 [aweight=wt] if (year == 2017 & scountry == "Greece") | (year == 2019 & scountry == "Cyprus")
local goodmean = `r(mean)'
local goodsd = `r(sd)'
local years "2017 2019 1 2007 2011 2022" 
replace year = 1 if scountry == "Cyprus" & year == 2017
foreach i of local years {
	levelsof scountry if !missing(Euro_corrlocgov) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 2017 & "`country'" == "Greece") | (`i' == 2019 & "`country'" == "Cyprus") {
			quietly summ WVS_q115 [aweight=wt] if scountry == "`country'" & year == `i'
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ Euro_corrlocgov [aweight=wt] if scountry == "`country'" & year == `i'
			generate Ppl2SevMu`country' = `goodmeani' - `r(mean)' if scountry == "`country'"
			generate Ppl2SevSigma`country' = `goodsdi' if scountry == "`country'"
			replace Euro_corrlocgov = (((Euro_corrlocgov - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else if (`i' != 2017 & "`country'" == "Greece") | (`i' != 2019 & "`country'" == "Cyprus") {
			quietly summ Euro_corrlocgov [aweight=wt] if scountry == "`country'" & year == `i'
			replace Euro_corrlocgov = (((Euro_corrlocgov - `r(mean)') / `r(sd)') * Ppl2SevSigma`country') + `r(mean)' + Ppl2SevMu`country' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ Euro_corrlocgov [aweight=wt] if scountry == "`country'" & year == `i'
			replace Euro_corrlocgov = (((Euro_corrlocgov - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}
replace year = 2017 if scountry == "Cyprus" & year == 1

generate corrlocgov = .
local corrlocgovs "Afro_corrloccouncil Afro7_Q44D Afro8_Q42D Latin_corrgovcouncil Latin_corrgovloc Latin_corrlocalgov AFTimor_m47_04 TIDRC_CORRPEOPLE4FIN TI_Q6_5 AFAfghan_x23c Asia_q117 MAsia_q116 WVS_q115 Euro_corrlocgov"
foreach corrlocgov of local corrlocgovs {
	replace corrlocgov = `corrlocgov' if !missing(`corrlocgov')
	drop `corrlocgov'
}
drop Ppl2*

// Petty: Authorities
replace Afro_bribepolice = 0 if Afro_bribepolice != 1 & !missing(Afro_bribepolice)
replace Afro7_Q49T = 0 if Afro7_Q49T != 1 & !missing(Afro7_Q49T)
replace Afro8_Q44N = 0 if Afro8_Q44N != 1 & !missing(Afro8_Q44N)
replace TIDRC_BRIBE5FIN = 0 if TIDRC_BRIBE5FIN != 1 & !missing(TIDRC_BRIBE5FIN)
replace TI_Q9_5 = 0 if TI_Q9_5 != 1 & !missing(TI_Q9_5)

generate QoG_bribepolice = QoG2013_q16_3
replace QoG_bribepolice = QoG2017_Q18_3 if missing(QoG_bribepolice)
replace QoG_bribepolice = QoG2021_q19_3 if missing(QoG_bribepolice)
drop QoG2013_q16_3 QoG2017_Q18_3 QoG2021_q19_3
quietly summ QoG_bribepolice [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local badmean = `r(mean)'
quietly summ Euro_bribepolice [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(QoG_bribepolice), local(years)
foreach i of local years {
	levelsof scountry if !missing(QoG_bribepolice) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Belgium" | "`country'" == "Bulgaria" | "`country'" == "Czech Republic" | "`country'" == "Denmark" | "`country'" == "Finland" | "`country'" == "France" | "`country'" == "Germany" | "`country'" == "Greece" | "`country'" == "Hungary" | "`country'" == "Italy" | "`country'" == "Netherlands" | "`country'" == "Poland" | "`country'" == "Portugal" | "`country'" == "Romania" | "`country'" == "Slovakia" | "`country'" == "Spain" | "`country'" == "Sweden") {
			quietly summ Euro_bribepolice [aweight=wt] if scountry == "`country'" & year == 2017
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ QoG_bribepolice [aweight=wt] if scountry == "`country'" & year == 2017
			local badmeani = `r(mean)'
			quietly summ QoG_bribepolice [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribepolice = (((QoG_bribepolice - `r(mean)') / `r(sd)') * `goodsdi') + `r(mean)' + `goodmeani' - `badmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ QoG_bribepolice [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribepolice = (((QoG_bribepolice - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

generate bribepolice = .
local bribepolices = "Afro_bribepolice Afro7_Q49T Afro8_Q44N QoG_bribepolice LAPOP_exc2 TIDRC_BRIBE5FIN TI_Q9_5 Euro_bribepoliceyesno"
foreach bribepolice of local bribepolices {
	replace bribepolice = `bribepolice' if !missing(`bribepolice')
	drop `bribepolice'
}

// Petty: Government Officials

quietly summ WVS_q118 [aweight=wt] if scountry == "Germany"
local badmean = `r(mean)'
quietly summ Euro_bribegovoffyesno [aweight=wt] if scountry == "Germany"
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(WVS_q118), local(years)
foreach i of local years {
	levelsof scountry if !missing(WVS_q118) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Germany") {
			quietly summ Euro_bribegovoffyesno [aweight=wt] if scountry == "`country'"
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			quietly summ WVS_q118 [aweight=wt] if scountry == "`country'" & year == `i'
			replace WVS_q118 = (((WVS_q118 - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			quietly summ WVS_q118 [aweight=wt] if scountry == "`country'" & year == `i'
			replace WVS_q118 = (((WVS_q118 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

levelsof year if !missing(WBES_corr4), local(years)
foreach i of local years {
	levelsof scountry if !missing(WBES_corr4) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ WBES_corr4 [aweight=wt] if scountry == "`country'" & year == `i'
		replace WBES_corr4 = (((WBES_corr4 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}
 
quietly summ ISSP_v62 [aweight=wt] if (scountry == "Argentina" | scountry == "Dominican Republic") & year == 2006
local badmean = `r(mean)'
quietly summ LAPOP_exc6 [aweight=wt] if (scountry == "Argentina" | scountry == "Dominican Republic") & year == 2008
local goodmean = `r(mean)'
local goodsd = `r(sd)'
display "The average adjustment from people to severity in corrstate is: `goodmean' - `badmean'"

levelsof year if !missing(ISSP_v62), local(years)
foreach i of local years {
	levelsof scountry if !missing(ISSP_v62) & year == `i', local(countries)
	foreach country of local countries {
		quietly summ ISSP_v62 [aweight=wt] if scountry == "`country'" & year == `i'
		replace ISSP_v62 = (((ISSP_v62 - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
	}
}

gen bribegovoff = .
local bribegovoffs "LAPOP_exc6 WVS_q118 WBES_corr4 ISSP_v62 Euro_bribegovoffyesno"
foreach bribegovoff of local bribegovoffs {
	replace bribegovoff = `bribegovoff' if !missing(`bribegovoff')
	drop `bribegovoff'
}

// Petty: Judicial
gen bribecourt = .
replace TI_Q9_6 = 0 if TI_Q9_6 != 1 & !missing(TI_Q9_6)
local bribecourts "LAPOP_exc14 TI_Q9_6 Euro_bribejudgeyesno"
foreach bribecourt of local bribecourts {
	replace bribecourt = `bribecourt' if !missing(`bribecourt')
	drop `bribecourt'
}

// Petty: Utilities

gen bribeutilities = .
local bribeutilitiess "WBES_corr6 TI_Q9_4 Afro_bribewatersan TIDRC_BRIBE4FIN"
foreach bribeutilities of local bribeutilitiess {
	replace bribeutilities = `bribeutilities' if !missing(`bribeutilities')
	drop `bribeutilities'
}

// Petty: Education
replace Afro_bribeschool = 0 if Afro_bribeschool != 1 & !missing(Afro_bribeschool)
replace Afro_bribeschoolserv = 0 if Afro_bribeschoolserv != 1 & !missing(Afro_bribeschoolserv)
replace Afro7_Q49C = 0 if Afro7_Q49C != 1 & !missing(Afro7_Q49C)
replace Afro8_Q44C = 0 if Afro8_Q44C != 1 & !missing(Afro8_Q44C)
replace TIDRC_BRIBE1FIN = 0 if TIDRC_BRIBE1FIN != 1 & !missing(TIDRC_BRIBE1FIN)
replace TI_Q9_1 = 0 if TI_Q9_1 != 1 & !missing(TI_Q9_1)

generate QoG_bribeeduc = QoG2013_q16_1
replace QoG_bribeeduc = QoG2017_Q18_1 if missing(QoG_bribeeduc)
replace QoG_bribeeduc = QoG2021_q19_1 if missing(QoG_bribeeduc)
drop QoG2013_q16_1 QoG2017_Q18_1 QoG2021_q19_1
summ QoG_bribeeduc [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local badmean = `r(mean)'
summ Euro_bribeeduc [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
levelsof year if !missing(QoG_bribeeduc), local(years)
foreach i of local years {
	levelsof scountry if !missing(QoG_bribeeduc) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Belgium" | "`country'" == "Bulgaria" | "`country'" == "Czech Republic" | "`country'" == "Denmark" | "`country'" == "Finland" | "`country'" == "France" | "`country'" == "Germany" | "`country'" == "Greece" | "`country'" == "Hungary" | "`country'" == "Italy" | "`country'" == "Netherlands" | "`country'" == "Poland" | "`country'" == "Portugal" | "`country'" == "Romania" | "`country'" == "Slovakia" | "`country'" == "Spain" | "`country'" == "Sweden") {
			summ Euro_bribeeduc [aweight=wt] if scountry == "`country'" & year == 2017
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			summ QoG_bribeeduc [aweight=wt] if scountry == "`country'" & year == 2017
			local badmeani = `r(mean)'
			summ QoG_bribeeduc [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribeeduc = (((QoG_bribeeduc - `r(mean)') / `r(sd)') * `goodsdi') + `r(mean)' + `goodmeani' - `badmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			summ QoG_bribeeduc [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribeeduc = (((QoG_bribeeduc - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

generate bribeschool = .
local bribeschools = "Afro_bribeschool Afro_bribeschoolserv Afro7_Q49C Afro8_Q44C QoG_bribeeduc LAPOP_exc16 TIDRC_BRIBE1FIN TI_Q9_1 Euro_bribeeducyesno"
foreach bribeschool of local bribeschools {
	replace bribeschool = `bribeschool' if !missing(`bribeschool')
	drop `bribeschool'
}

// Petty: Documents
replace Afro7_Q49K = 0 if Afro7_Q49K != 1 & !missing(Afro7_Q49K)
replace Afro_bribedocper = 0 if Afro_bribedocper != 1 & !missing(Afro_bribedocper)
replace Afro8_Q44I = 0 if Afro8_Q44I != 1 & !missing(Afro8_Q44I)
replace TIDRC_BRIBE3FIN = 0 if TIDRC_BRIBE3FIN != 1 & !missing(TIDRC_BRIBE3FIN)
replace TI_Q9_3 = 0 if TI_Q9_3 != 1 & !missing(TI_Q9_3)
generate bribedoc = .
local bribedocs "Afro7_Q49K Afro_bribedocper Afro8_Q44I TIDRC_BRIBE3FIN LAPOP_exc11 TI_Q9_3 WBES_corr8"
foreach bribedoc of local bribedocs {
	summ `bribedoc'
	replace bribedoc = `bribedoc' if !missing(`bribedoc')
	drop `bribedoc'
}

// Petty: Health
drop QoG2017_Q17_2 QoG2021_q18_2
replace Afro7_Q49G = 0 if Afro7_Q49G != 1 & !missing(Afro7_Q49G)
replace Afro8_Q44F = 0 if Afro8_Q44F != 1 & !missing(Afro8_Q44F)
replace Afro_bribehealth = 0 if Afro_bribehealth != 1 & !missing(Afro_bribehealth)
replace TI_Q9_2 = 0 if TI_Q9_2 != 1 & !missing(TI_Q9_2)
replace TIDRC_BRIBE2FIN = 0 if TIDRC_BRIBE2FIN != 1 & !missing(TIDRC_BRIBE2FIN)

generate QoG_bribehealth = QoG2013_q16_2
replace QoG_bribehealth = QoG2017_Q18_2 if missing(QoG_bribehealth)
replace QoG_bribehealth = QoG2021_q19_2 if missing(QoG_bribehealth)
drop QoG2013_q16_2 QoG2017_Q18_2 QoG2021_q19_2
summ QoG_bribehealth [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local badmean = `r(mean)'
summ Euro_bribehealth [aweight=wt] if (year == 2017 & (scountry == "Belgium" | scountry == "Bulgaria" | scountry == "Czech Republic" | scountry == "Denmark" | scountry == "Finland" | scountry == "France" | scountry == "Germany" | scountry == "Greece" | scountry == "Hungary" | scountry == "Italy" | scountry == "Netherlands" | scountry == "Poland" | scountry == "Portugal" | scountry == "Romania" | scountry == "Slovakia" | scountry == "Spain" | scountry == "Sweden"))
local goodmean = `r(mean)'
local goodsd = `r(sd)'
display "The average adjustment from you to family in bribepolice is: `goodmean' - `badmean'"
levelsof year if !missing(QoG_bribehealth), local(years)
foreach i of local years {
	levelsof scountry if !missing(QoG_bribehealth) & year == `i', local(countries)
	foreach country of local countries {
		if ("`country'" == "Belgium" | "`country'" == "Bulgaria" | "`country'" == "Czech Republic" | "`country'" == "Denmark" | "`country'" == "Finland" | "`country'" == "France" | "`country'" == "Germany" | "`country'" == "Greece" | "`country'" == "Hungary" | "`country'" == "Italy" | "`country'" == "Netherlands" | "`country'" == "Poland" | "`country'" == "Portugal" | "`country'" == "Romania" | "`country'" == "Slovakia" | "`country'" == "Spain" | "`country'" == "Sweden") {
			summ Euro_bribehealth [aweight=wt] if scountry == "`country'" & year == 2017 
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			summ QoG_bribehealth [aweight=wt] if scountry == "`country'" & year == 2017
			local badmeani = `r(mean)'
			summ QoG_bribehealth [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribehealth = (((QoG_bribehealth - `r(mean)') / `r(sd)') * `goodsdi') + `r(mean)' + `goodmeani' - `badmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			summ QoG_bribehealth [aweight=wt] if scountry == "`country'" & year == `i'
			replace QoG_bribehealth = (((QoG_bribehealth - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}

generate bribehealth = .
local bribehealths "Afro7_Q49G Afro8_Q44F QoG_bribehealth Afro_bribehealth TI_Q9_2 TIDRC_BRIBE2FIN LAPOP_exc15 Euro_bribehealthyesno"
foreach bribehealth of local bribehealths {
	summ `bribehealth'
	replace bribehealth = `bribehealth' if !missing(`bribehealth')
	drop `bribehealth'
}

// Petty: Elections

summ Afro_bribeelection [aweight=wt] if year == 2012 & (scountry == "Ghana" | scountry == "Zimbabwe")
local badmean = `r(mean)'
summ WVS_v228d [aweight=wt] if year == 2012 & (scountry == "Ghana" | scountry == "Zimbabwe")
local goodmean = `r(mean)'
local goodsd = `r(sd)'
display "The average adjustment from people to severity in corrstate is: `goodmean' - `badmean'"
levelsof year if !missing(Afro_bribeelection), local(years)
foreach i of local years {
	levelsof scountry if !missing(Afro_bribeelection) & year == `i', local(countries)
	foreach country of local countries {
		if (`i' == 2012 & ("`country'" == "Ghana" | "`country'" == "Zimbabwe")) {
			summ WVS_v228d [aweight=wt] if scountry == "`country'" & year == `i'
			local goodmeani = `r(mean)'
			local goodsdi = `r(sd)'
			summ Afro_bribeelection [aweight=wt] if scountry == "`country'" & year == `i'
			replace Afro_bribeelection = (((Afro_bribeelection - `r(mean)') / `r(sd)') * `goodsdi') + `goodmeani' if scountry == "`country'" & year == `i'
		}
		else { 
			summ Afro_bribeelection [aweight=wt] if scountry == "`country'" & year == `i'
			replace Afro_bribeelection = (((Afro_bribeelection - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if scountry == "`country'" & year == `i'
		}
	}
}
levelsof year if !missing(TI_Q19b), local(years)
foreach i of local years {
	levelsof scountry if !missing(TI_Q19b) & year == `i', local(countries)
	foreach country of local countries {
		summ TI_Q19b [aweight=wt] if year == `i' & scountry == "`country'"
		replace TI_Q19b = (((TI_Q19b - `r(mean)') / `r(sd)') * `goodsd') + `r(mean)' + `goodmean' - `badmean' if year == `i' & scountry == "`country'"
	}
}

generate bribeelec = .
local bribeelecs "Afro_bribeelection Afro_bribevoters QoG2013_q18 QoG2017_Q19 WVS_v228d WVS_q227 TI_Q19b"
foreach bribeelec of local bribeelecs {
	summ `bribeelec'
	replace bribeelec = `bribeelec' if !missing(`bribeelec')
	drop `bribeelec'
}

drop if missing(corrpres) & missing(corrparl) & missing(corrstate) & missing(corrjudge) & missing(corrtax) & missing(corrcivil) & missing(corrpolitician) & missing(corrgovoff) & missing(corrseverity) & missing(corrpolice) & missing(corrlocgov) & missing(bribepolice) & missing(bribegovoff) & missing(bribecourt) & missing(bribeutilities) & missing(bribeschool) & missing(bribedoc) & missing(bribehealth) & missing(bribeelec)


save "Temp\Masterfile Individual.dta", replace

// 5. Use imputation to fill up missing values. This allows for the building of the most stable version of the subnational corruption index.

use "Temp\Masterfile Individual.dta", clear
gen individ = _n
keep iso scountry wt regname regcode year Source Notes iso_code gdlcode regcurb reguname regnum corrpres-corrlocgov individ
drop if missing(corrpres) & missing(corrparl) & missing(corrstate) & missing(corrjudge) & missing(corrtax) & missing(corrcivil) & missing(corrpolitician) & missing(corrgovoff) & missing(corrseverity) & missing(corrpolice) & missing(corrlocgov)
summ corrpres-corrlocgov

mi set mlong
mi xtset, clear
mi register imputed corrpres-corrlocgov
mi impute chained (regress) corrpres-corrlocgov, add(1) // Maintains exact correlation structure.
drop if _mi_m == 0

pca corrpres-corrlocgov
predict grand
alpha corrpres-corrlocgov 

drop _mi_m-_mi_miss

save "Temp\Grand.dta", replace

use "Temp\Masterfile Individual.dta", clear
gen individ = _n
keep iso scountry wt regname regcode year Source Notes iso_code gdlcode regcurb reguname regnum bribepolice-bribeelec individ
drop if missing(bribepolice) & missing(bribegovoff) & missing(bribecourt) & missing(bribeutilities) & missing(bribeschool) & missing(bribedoc) & missing(bribehealth) & missing(bribeelec)

mi set mlong
mi xtset, clear
mi register imputed bribepolice-bribeelec
mi impute chained (regress) bribepolice-bribeelec, add(1)
drop if _mi_m == 0

pca bribepolice-bribeelec
predict petty
alpha bribepolice-bribeelec

drop _mi_m-_mi_miss
save "Temp\Petty.dta", replace
use "Temp\Petty.dta", clear
append using "Temp\Grand.dta"

save "Temp\IndividualAppended.dta", replace

use "Temp\IndividualAppended.dta", clear
// Create N here:

bys regcode year Source: egen NGrandx = count(regcode) if !missing(grand)
bys regcode year Source: egen NPettyx = count(regcode) if !missing(petty)

by regcode year: egen NGrand = count(iso) if !missing(grand)
by regcode year: egen NPetty = count(iso) if !missing(petty)

tab Source
replace Source = "The Asia Foundation" if iso == "TLS"
replace Source = "Eurobarometers" if strpos(Source, "Eurobarometers") != 0

collapse (first) regname (min) NGrand NPetty NGrandx NPettyx (mean) bribepolice-grand [aweight=wt], by(scountry iso regcode year Source) fast

gen GrandWeight = NGrandx / NGrand
gen PettyWeight = NPettyx / NPetty

local factors "corrpres corrparl corrstate corrjudge corrtax corrcivil corrpolitician corrgovoff corrseverity corrpolice corrlocgov grand"
foreach fac of local factors {
	gen `fac'w = `fac' * GrandWeight
	drop `fac'
	bys regcode year: egen `fac' = sum(`fac'w)
	drop `fac'w
	replace `fac' = . if missing(GrandWeight)
}
local factors "bribepolice bribegovoff bribecourt bribeutilities bribeschool bribedoc bribehealth bribeelec petty"
foreach fac of local factors {
	gen `fac'w = `fac' * PettyWeight
	drop `fac'
	bys regcode year: egen `fac' = sum(`fac'w)
	drop `fac'w
	replace `fac' = . if missing(PettyWeight)
}

by regcode year, sort: gen source = Source if _n == 1
by regcode year: replace source = source[_n-1] + ", " + Source if _n > 1 & !missing(Source)
by regcode year: replace source = source[_N]
drop Source

collapse (first) regname source (min) NGrand NPetty (min) corrpres-petty, by(scountry iso regcode year) fast

replace NGrand = 0 if missing(NGrand)
replace NPetty = 0 if missing(NPetty)

export excel "Temp\TempFile.xlsx", firstrow(variables) replace
import excel "Temp\TempFile.xlsx", firstrow clear
egen countryid = group(iso)
egen id = group(regcode)
xtset id year

// 6. Outlier treatment - Diminish subnational variation around national average, based on sample size. Winsorize extremes.
reg grand i.countryid i.year i.countryid#i.year
predict cwithingrandres, residual
reg petty i.countryid i.year i.countryid#i.year
predict cwithinpettyres, residual

replace grand = (grand - cwithingrandres) + 0.55 if cwithingrandres >= 0.55
replace grand = (grand - cwithingrandres) - 0.55 if cwithingrandres <= -0.55
replace petty = (petty - cwithinpettyres) + 0.55 if cwithinpettyres >= 0.55
replace petty = (petty - cwithinpettyres) - 0.55 if cwithinpettyres <= -0.55

twoway scatter cwithingrandres NGrand, yline(-0.55 0.55)
twoway scatter cwithinpettyres NPetty, yline(-0.55 0.55)

xtreg grand, fe
predict grandresidual, e
predict grandwithin, xbu
xtreg petty, fe
predict pettywithin, xbu
predict pettyresidual, e

twoway scatter grandresidual NGrand, yline(-0.55 0.55)
reg grandresidual NGrand c.NGrand#c.NGrand if grandresidual < 0 & NGrand > 1100

twoway scatter pettyresidual NPetty, yline(-0.55 0.55)
reg pettyresidual NPetty c.NPetty#c.NPetty if pettyresidual < 0 & NPetty > 1100

replace grand = (grand - grandresidual) + 0.55 if grandresidual >= 0.55
replace grand = (grand - grandresidual) - 0.55 if grandresidual <= -0.55
replace petty = (petty - pettyresidual) + 0.55 if pettyresidual >= 0.55
replace petty = (petty - pettyresidual) - 0.55 if pettyresidual <= -0.55 // 10%

// Account for potential bias of 1-year regions (residual = 0):
// Grand:
encode source, gen(isource)
reg grandresidual c.NGrand c.NGrand#c.NGrand i.isource if grandresidual > 0
predict pgrandresidual if grandresidual == 0, xb
reg grandresidual c.NGrand c.NGrand#c.NGrand i.isource if grandresidual < 0
predict ngrandresidual if grandresidual == 0, xb

replace grand = grand - pgrandresidual if scountry == "Angola" | scountry == "Albania" | scountry == "Andorra" | scountry == "Armenia" | scountry == "Azerbaijan" | scountry == "Belarus" | scountry == "Cook Islands" | scountry == "Ethiopia" | scountry == "Fiji" | scountry == "Georgia" | scountry == "Iran" | scountry == "Kazakhstan" | scountry == "Kyrgyzstan" | scountry == "Cambodia" | scountry == "Kuwait" | scountry == "Sri Lanka" | scountry == "Moldova" | scountry == "Marshall Islands" | scountry == "North Macedonia" | scountry == "Malta" | scountry == "Myanmar" | scountry == "Malawi" | scountry == "Niue" | scountry == "Nauru" | scountry == "Pakistan" | scountry == "Philippines" | scountry == "Palau" | scountry == "PNG" | scountry == "Sierra Leone" | scountry == "South Sudan" | scountry == "São Tomé and Príncipe" | scountry == "Chad" | scountry == "Tajikistan" | scountry == "Tokelau" | scountry == "Turkmenistan" | scountry == "Tonga" | scountry == "Turkey" | scountry == "Tuvalu" | scountry == "Vanuatu" | scountry == "Samoa" | scountry == "Central African Republic" | scountry == "Uzbekistan" | scountry == "Somalia" // These are countries that I expect are overestimated in our index, that is their corruption is higher in reality than we estimate.
replace grand = grand - ngrandresidual if scountry == "Congo Democratic Republic" | scountry == "Bhutan" | scountry == "FSM" | scountry == "Israel" | scountry == "Kiribati" | scountry == "New Caledonia" | scountry == "Norway" | scountry == "Singapore" | scountry == "French Polynesia" | scountry == "Solomon Islands" | scountry == "Serbia" | scountry == "Seychelles"

reg pettyresidual c.NPetty c.NPetty#c.NPetty i.isource if pettyresidual > 0
predict ppettyresidual if pettyresidual == 0, xb
reg pettyresidual c.NPetty c.NPetty#c.NPetty i.isource if pettyresidual < 0
predict npettyresidual if pettyresidual == 0, xb

replace petty = petty - ppettyresidual if (scountry == "Angola" | scountry == "Andorra" | scountry == "Azerbaijan" | scountry == "Bangladesh" | scountry == "Cook Islands" | scountry == "Ethiopia" | scountry == "Fiji" | scountry == "Georgia" | scountry == "Indonesia" | scountry == "Iran" | scountry == "Kuwait" | scountry == "Lebanon" | scountry == "Libya" | scountry == "Malta" | scountry == "Myanmar" | scountry == "Malawi" | scountry == "Niue" | scountry == "Nauru" | scountry == "Palau" | scountry == "Palestine" | scountry == "Rwanda" | scountry == "Solomon Islands" | scountry == "São Tomé and Príncipe" | scountry == "Suriname" | scountry == "Tajikistan" | scountry == "Turkey" | scountry == "Tuvalu" | scountry == "Yemen" | scountry == "Bahamas") & !missing(ppettyresidual) // These are countries that I expect are overestimated in our index, that is their corruption is higher in reality than we estimate.
replace petty = petty - npettyresidual if (scountry == "Barbados" | scountry == "Canada" | scountry == "Switzerland" | scountry == "Congo Democratic Republic" | scountry == "FSM" | scountry == "Israel" | scountry == "Kiribati" | scountry == "Sri Lanka" | scountry == "Marshall Islands" | scountry == "New Caledonia" | scountry == "Norway" | scountry == "PNG" | scountry == "Puerto Rico" | scountry == "French Polynesia" | scountry == "Sierra Leone" | scountry == "Serbia" | scountry == "Tokelau" | scountry == "Tonga" | scountry == "Vietnam" | scountry == "Vanuatu" | scountry == "Samoa" | scountry == "PuertoRico") & !missing(npettyresidual)

pwcorr petty grand

// Predict missing grand/petty
reg grand c.petty i.id
predict GrandPredicted, xb
reg petty c.grand i.id
predict PettyPredicted, xb
gen grand2 = grand
replace grand2 = GrandPredicted if missing(grand2)
gen petty2 = petty
replace petty2 = PettyPredicted if missing(petty2)
pwcorr grand GrandPredicted petty PettyPredicted
drop GrandPredicted PettyPredicted

summ grand
replace grand = (grand - `r(min)') / (`r(max)' - `r(min)') * 100
summ petty
replace petty = (petty - `r(min)') / (`r(max)' - `r(min)') * 100
generate SCI = (petty + grand)/2

summ grand2
replace grand2 = (grand2 - `r(min)') / (`r(max)' - `r(min)') * 100
summ petty2
replace petty2 = (petty2 - `r(min)') / (`r(max)' - `r(min)') * 100

generate SCI2 = (petty2 + grand2) / 2
drop corrpres-corrlocgov bribepolice-bribeelec

sort regcode year

order scountry year iso regcode regname source SCI SCI2 grand grand2 petty petty2 NGrand NPetty
keep scountry year iso regcode regname source SCI SCI2 grand grand2 petty petty2 NGrand NPetty

save "Temp\SCI_SubNational.dta", replace

// 7. Create national version by weighting the subnational version.

use "Temp\IndividualAppended.dta", clear

bys iso year: egen NGrand = count(iso) if !missing(grand)
bys iso year: egen NPetty = count(iso) if !missing(petty)

keep iso scountry wt regname regcode year NGrand NPetty

merge m:1 regcode year using "Temp\SCI_SubNational.dta"

collapse (first) iso source (min) NGrand NPetty (mean) grand grand2 petty petty2 [aweight=wt], by(scountry year) fast

pwcorr petty grand

generate SCI = (petty + grand)/2
generate SCI2 = (petty2 + grand2)/2
pwcorr petty2 grand2

sort iso year

//drop regcode
generate regcode = iso + "t"

export excel "Temp\TempFile.xlsx", firstrow(variables) replace
import excel "Temp\TempFile.xlsx", firstrow clear

save "Temp\SCI_National.dta", replace

use "Temp\SCI_SubNational.dta", clear
append using "Temp\SCI_National"

recast double SCI SCI2 grand grand2 petty petty2

local roundvars "SCI SCI2 grand grand2 petty petty2"
foreach rvar of local roundvars {
	replace `rvar' = round(`rvar', 0.1)
}

rename regcode GDLcode
rename regname region

label variable scountry "Country Name"
label variable year "Year"
label variable iso "3-letter ISO Code"
label variable GDLcode "Global Data Lab Region Code"
label variable region "Global Data Lab Region Name"
label variable source "Raw Datasources"
label variable SCI "Subnational Corruption Index, No Prediction"
label variable SCI2 "Subnational Corruption Index, Prediction"
label variable grand "Subnational Grand Index, No Prediction"
label variable grand2 "Subnational Grand Index, Prediction"
label variable petty "Subnational Petty Index, No Prediction"
label variable petty2 "Subnational Petty Index, Prediction"
label variable NGrand "Number of Respondents, Grand Index"
label variable NPetty "Number of Respondents, Petty Index"

gen minyear = year * -1
sort iso minyear GDLcode
drop minyear

save "Baseline Dataset SCI.dta", replace
export excel "Baseline Dataset SCI.xlsx", replace firstrow(variables)
savespss "Baseline Dataset SCI.sav", replace
