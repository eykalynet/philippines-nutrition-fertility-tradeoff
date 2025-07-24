********************************************************;
// Imports the DHS ARCGIS data into STATA for 2003 and 2008.
// Generates the recovered correponding barangay, municipality, and province.
// Make geocodes in DHS conform to POEA setup. 
********************************************************;
cap log close
clear all
set more off
set maxvar 32600

********************************************************;


** IMPORT ARCGIS FILES

	* 2003
	shp2dta using "$input2003", database(usdb03) coordinates(uscoord03) genid(id03) replace
	
	* 2008 
	shp2dta using "$input2008", database(usdb08) coordinates(uscoord08) genid(id08) replace

	
** IMPORT GPS COORDINATES INTO STATA
	* 2003
	import excel  using "$geodata/2003DHS_geocodes_bycluster.xlsx", firstrow

	rename Region province
	rename Province muni

	replace province = strupper(province)
	replace province = strtrim(province)
	
	replace muni = strupper(muni)
	replace muni = strtrim(muni)
	
	drop X 
	
	save "$geodata/2003geocodes.dta", replace
	
	*2008
	clear 
	import excel  using "$geodata/2008DHS_geocodes_bycluster.xlsx", firstrow

	rename Region province
	rename Province muni

	replace province = strupper(province)
	replace province = strtrim(province)
	
	replace muni = strupper(muni)
	replace muni = strtrim(muni)
	
	save "$geodata/2008geocodes.dta", replace
	
** APPEND DHS GEO CODES ACROSS WAVES
	
	use "$geodata/2003geocodes.dta", clear
	
	append using "$geodata/2008geocodes.dta"
	drop if DHSYEAR == . 
	
	save "$geodata/2003_2008geocodes.dta", replace
	
** MATCH REGION/PROVINCE/MUNICIPALITY geocodes
	
	** Make DHS conform to POEA geocodes (REGION)
	gen regn = 1 		if DHSREGCO == 3 & DHSYEAR == 2003
	replace regn = 2 	if DHSREGCO == 4 & DHSYEAR == 2003
	replace regn = 3	if DHSREGCO == 5 & DHSYEAR == 2003
	replace regn = 4	if DHSREGCO == 6 & DHSYEAR == 2003
	replace regn = 5	if DHSREGCO == 8 & DHSYEAR == 2003
	replace regn = 6 	if DHSREGCO == 9 & DHSYEAR == 2003
	replace regn = 7 	if DHSREGCO == 10 & DHSYEAR == 2003
	replace regn = 8	if DHSREGCO == 11 & DHSYEAR == 2003
	replace regn = 9	if DHSREGCO == 12 & DHSYEAR == 2003
	replace regn = 10 	if DHSREGCO == 13 & DHSYEAR == 2003
	replace regn = 11 	if DHSREGCO == 14 & DHSYEAR == 2003
	replace regn = 12	if DHSREGCO == 15 & DHSYEAR == 2003
	replace regn = 13	if DHSREGCO == 1 & DHSYEAR == 2003
	replace regn = 14 	if DHSREGCO == 2 & DHSYEAR == 2003
	replace regn = 15 	if DHSREGCO == 17 & DHSYEAR == 2003
	replace regn = 16 	if DHSREGCO == 16 & DHSYEAR == 2003
	replace regn = 17 	if DHSREGCO == 7 & DHSYEAR == 2003
	
	replace regn = 1 	if DHSREGCO == 1 & DHSYEAR == 2008
	replace regn = 2 	if DHSREGCO == 2 & DHSYEAR == 2008
	replace regn = 3	if DHSREGCO == 3 & DHSYEAR == 2008
	replace regn = 4	if DHSREGCO == 41 & DHSYEAR == 2008
	replace regn = 5	if DHSREGCO == 5 & DHSYEAR == 2008
	replace regn = 6 	if DHSREGCO == 6 & DHSYEAR == 2008
	replace regn = 7 	if DHSREGCO == 7 & DHSYEAR == 2008
	replace regn = 8	if DHSREGCO == 8 & DHSYEAR == 2008
	replace regn = 9	if DHSREGCO == 9 & DHSYEAR == 2008
	replace regn = 10 	if DHSREGCO == 10 & DHSYEAR == 2008
	replace regn = 11 	if DHSREGCO == 11 & DHSYEAR == 2008
	replace regn = 12	if DHSREGCO == 12 & DHSYEAR == 2008
	replace regn = 13	if DHSREGCO == 14 & DHSYEAR == 2008
	replace regn = 14 	if DHSREGCO == 15 & DHSYEAR == 2008
	replace regn = 15 	if DHSREGCO == 16 & DHSYEAR == 2008
	replace regn = 16 	if DHSREGCO == 13 & DHSYEAR == 2008
	replace regn = 17 	if DHSREGCO == 42 & DHSYEAR == 2008
	
	** Make DHS conform to POEA geocodes (PROVINCE)
	
	gen prv = 28 		if province == "ILOCOS NORTE" & regn == 1
	replace prv = 29 	if province == "ILOCOS SUR" & regn == 1
	replace prv = 33 	if province == "LA UNION" & regn == 1
	replace prv = 55 	if province == "PANGASINAN" & regn == 1
	
	replace prv = 15 	if province == "CAGAYAN" & regn == 2
	replace prv = 15 	if province == "" & regn == 2 & muni == "IGUIG"
	replace prv = 15 	if province == "" & regn == 2 & muni == "LAL-LO"
	replace prv = 15 	if province == "" & regn == 2 & muni == "PAMPLONA"
	replace province = "ISABELA" if DHSYEAR == 2008 & DHSCLUST == 231 
	replace province = "NUEVA VIZCAYA" if DHSYEAR == 2008 & DHSCLUST ==235
	replace prv = 31	if province == "ISABELA" & regn == 2
	replace prv = 50	if province == "NUEVA VIZCAYA" & regn == 2
	replace prv = 57	if province == "QUIRINO" & regn == 2
	replace prv = 9 	if province == "BATANES" & regn == 2
	
	replace prv = 8		if province == "BATAAN" & regn == 3
	replace province = "BULACAN" if province == "PAMPANGA" & DHSYEAR == 2003 & DHSCLUST == 77
	replace province = "BULACAN" if province == "METRO MANILA" & DHSYEAR == 2003 & DHSCLUST == 82
	replace province = "BULACAN" if province == "METRO MANILA" & DHSYEAR == 2008 & (DHSCLUST == 251 | DHSCLUST == 252 | DHSCLUST == 253 | DHSCLUST == 254)
	replace province = "BULACAN" if province == "BULAKAN" & regn == 3
	replace prv = 14	if province == "BULACAN" & regn == 3
	replace prv = 49	if province == "NUEVA ECIJA" & regn == 3
	replace province = "PAMPANGA" if DHSCLUST == 109 & DHSYEAR == 2003
	replace muni = "MACABEBE" if DHSCLUST == 109 & DHSYEAR == 2003
	replace prv = 54	if province == "PAMPANGA" & regn == 3
	replace prv = 69	if province == "TARLAC" & regn == 3
	replace prv = 71	if province == "ZAMBALES" & regn == 3
	replace prv = 77	if province == "AURORA" & regn == 3
	
	replace province = "CAVITE" if DHSYEAR == 2008 & (DHSCLUST >=311 & DHSCLUST <=316)
	replace province = "LAGUNA" if DHSYEAR == 2008 & DHSCLUST == 344
	replace province = "RIZAL" if DHSYEAR == 2008 & DHSCLUST == 366
	replace prv = 10	if province == "BATANGAS" & regn == 4
	replace prv = 21	if province == "CAVITE" & regn == 4
	replace prv = 34	if province == "LAGUNA" & regn == 4
	replace prv = 56	if province == "QUEZON" & regn == 4
	replace prv = 58	if province == "RIZAL" & regn == 4
	
	replace province = "CAMARINES NORTE" if (DHSCLUST ==418 | DHSCLUST ==419) & DHSYEAR ==2008
	replace province = "SORSOGON" if DHSYEAR == 2008 & DHSCLUST == 444
	replace prv = 5		if province == "ALBAY" & regn == 5
	replace prv = 16	if province == "CAMARINES NORTE" & regn == 5
	replace prv = 17	if province == "CAMARINES SUR" & regn == 5
	replace prv = 20	if province == "CATANDUANES" & regn == 5
	replace prv = 41	if province == "MASABATE" & regn == 5
	replace prv = 41	if province == "MASBATE" & regn == 5
	replace prv = 62	if province == "SORSOGON" & regn == 5
	
	replace province = "ILOILO" if DHSCLUST == 459 & DHSYEAR == 2008
	replace prv = 4		if province == "AKLAN" & regn == 6
	replace prv = 6		if province == "ANTIQUE" & regn == 6
	replace prv = 30	if province == "ILOILO" & regn == 6
	replace prv = 19	if province == "CAPIZ" & regn == 6
	replace prv = 45	if province == "NEGROS OCCIDENTAL" & regn == 6
	replace prv = 45	if province == "6101 NEGROS OCCIDENTAL" & regn == 6
	replace prv = 79	if province == "GUIMARAS" & regn == 6

	replace prv = 12	if province == "BOHOL" & regn == 7
	replace prv = 22	if province == "CEBU" & regn == 7
	replace prv = 22	if province == "6000 CEBU" & regn == 7
	replace prv = 22	if province == "6014 CEBU" & regn == 7
	replace prv = 46	if province == "NEGROS ORIENTAL" & regn == 7
	replace prv = 46	if province == "6219 NEGROS ORIENTAL" & regn == 7
	replace prv = 61	if province == "SIQUIJOR" & regn == 7

	replace province = "LEYTE" if DHSCLUST == 547 & DHSYEAR == 2008
	replace province = "NORTHERN SAMAR" if DHSCLUST == 561 & DHSYEAR == 2008
	replace prv = 26	if province == "EASTERN SAMAR" & regn == 8 
	replace muni = "GUIUAN" if province == "GUIUAN" & regn == 8
	replace prv = 26	if province == "GUIUAN" & regn == 8 
	replace prv = 37	if province == "LEYTE" & regn == 8 
	replace muni = "BAYBAY CITY" if province == "BAYBAY CITY" & regn == 8
	replace prv = 37	if province == "BAYBAY CITY" & regn == 8 
	replace province = "SAMAR" if province == "NORTHERN SAMAR" & muni == "CALBAYOG CITY"
	replace prv = 48	if province == "NORTHERN SAMAR" & regn == 8 
	replace prv = 60	if province == "SAMAR" & regn == 8 
	replace prv = 64	if province == "SOUTHERN LEYTE" & regn == 8 
	replace prv = 78	if province == "BILIRAN" & regn == 8 

	replace muni = "MOLAVE" if province == "MOLAVE"
	replace province = "ZAMBOANGA DEL SUR" 	if province == "MOLAVE" & regn == 9
	replace province = "ZAMBOANGA DEL SUR" if province == "ZAMBOANGA DEL NORTE" & muni == "AURORA"
	replace muni = "ZAMBOANGA" if DHSCLUST == 445 & DHSYEAR == 2003
	replace muni = "ZAMBOANGA" if DHSCLUST == 612 & DHSYEAR == 2008
	replace province = "ZAMBOANGA DEL NORTE" if DHSCLUST == 580 & DHSYEAR == 2008
	replace province = "ZAMBOANGA DEL SUR" if DHSYEAR == 2008 & (DHSCLUST == 587 | DHSCLUST == 589)
	replace province = "ZAMBOANGA SIBUGAY" if DHSCLUST == 609 & DHSYEAR == 2008
	replace muni = "BUUG" if DHSCLUST == 609 & DHSYEAR == 2008

	replace province = "ZAMBOANGA DEL SUR" if muni == "ZAMBOANGA"
	replace prv = 72	if province == "ZAMBOANGA DEL NORTE" & regn == 9
	replace prv = 73	if province == "ZAMBOANGA DEL SUR" & regn == 9
	replace prv = 83	if province == "ZAMBOANGA SIBUGAY" & regn == 9
	
	replace province = "LANAO DEL NORTE" if (DHSCLUST == 629 |DHSCLUST == 630) & DHSYEAR == 2008
	replace province = "MISAMIS ORIENTAL" if DHSCLUST >= 469 & DHSCLUST <= 474 & DHSYEAR == 2003
	replace province = "MISAMIS ORIENTAL" if DHSCLUST >= 637 & DHSCLUST <= 650 & DHSYEAR == 2008
	replace province = "MISAMIS ORIENTAL" if muni == "CAGAYAN DE ORO" & regn == 10
	replace province = "LANAO DEL NORTE" if DHSYEAR == 2003 & DHSCLUST == 463
	replace province = "MISAMIS ORIENTAL" if DHSYEAR ==2003 & DHSCLUST == 475
	replace muni = "PANTAR" if DHSYEAR == 2003 & DHSCLUST == 463
	replace muni = "KINOGUITAN" if DHSYEAR ==2003 & DHSCLUST == 475
	replace prv = 13	if province == "BUKIDNON" & regn == 10 
	replace prv = 18	if province == "CAMIGUIN" & regn == 10 
	replace prv = 35	if province == "LANAO DEL NORTE" & regn == 10 
	replace prv = 42	if province == "MISAMIS OCCIDENTAL" & regn == 10 
	replace prv = 43	if province == "MISAMIS ORIENTAL" & regn == 10 
	
	replace province = "DAVAO DEL SUR" if (muni == "DAVAO CITY" | muni == "MAGSAYSAY") & province == "DAVAO DEL NORTE" & regn == 11
	replace province = "DAVAO DEL NORTE" if muni == "PANABO" & province == "DAVAO DEL SUR" & regn == 11
	replace prv = 23	if province == "DAVAO DEL NORTE" & regn == 11
	replace prv = 82	if province == "COMPOSTELA VALLEY" & regn == 11
	replace prv = 24	if province == "DAVAO DEL SUR" & regn == 11
	replace prv = 25	if province == "DAVAO ORIENTAL" & regn == 11
	
	replace province = "COTABATO" if (DHSCLUST == 693 | DHSCLUST == 700) & DHSYEAR == 2008
	replace province = "COTABATO" if (DHSCLUST == 774 | DHSCLUST == 775) & DHSYEAR == 2003
	replace prv = 47	if province == "COTABATO" & regn == 12
	replace prv = 63	if province == "SOUTH COTABATO" & regn == 12
	replace prv = 65	if province == "SULTAN KADARAT" & regn == 12
	replace prv = 80	if province == "SARANGANI" & regn == 12
	replace prv = 65	if province == "SULTAN KUDARAT" & regn == 12
	
	// Districts of Metro Manila
	// First District = Manila
		replace province = "METRO MANILA" if DHSYEAR == 2003 & ( DHSCLUST >= 564 & DHSCLUST <= 581)
		replace province = "METRO MANILA" if DHSYEAR == 2003 & (DHSCLUST == 642 | DHSCLUST == 643)
		replace muni = "MANILA" if DHSYEAR == 2003 & ( DHSCLUST >= 564 & DHSCLUST <= 581)
		replace muni = "MANILA" if DHSYEAR == 2003 & DHSCLUST == 643
		replace prv = 39 	if muni == "MANILA" & province == "METRO MANILA" & regn == 13 
		replace prv = 39 	if DHSYEAR == 2008 & (DHSCLUST == 8 )
		replace province = "METRO MANILA" 	if DHSYEAR == 2008 & (DHSCLUST == 8 )
		replace muni = "MANILA" 	if DHSYEAR == 2008 & (DHSCLUST == 8 )
		
	// Second District = Mandaluyong, Marikina, Pasig, Quezon City, San Juan
		replace province = "METRO MANILA" 	if DHSYEAR == 2003 & (DHSCLUST >=582 & DHSCLUST <=624)
		replace muni = "QUEZON CITY" 	if DHSYEAR == 2003 & (DHSCLUST >=582 & DHSCLUST <=624)
		replace province = "METRO MANILA" 	if DHSYEAR == 2003 & (DHSCLUST >=625 & DHSCLUST <=626)
		replace muni = "MANDALUYONG" 	if DHSYEAR == 2003 & (DHSCLUST >=625 & DHSCLUST <=626)
		replace province = "METRO MANILA" 	if DHSYEAR == 2003 & DHSCLUST ==630
		replace muni = "MARIKINA" 	if DHSYEAR == 2003 & DHSCLUST == 630
		replace province = "METRO MANILA" 	if DHSYEAR == 2008 & (DHSCLUST == 28 | DHSCLUST == 34 | DHSCLUST == 41)
		replace muni ="QUEZON CITY"	if DHSYEAR == 2008 & (DHSCLUST == 28 | DHSCLUST == 34 | DHSCLUST == 41 )
		replace province ="METRO MANILA"	if DHSYEAR == 2008 & (DHSCLUST >=80 & DHSCLUST <=86)
		replace muni ="QUEZON CITY"	if DHSYEAR == 2008 & (DHSCLUST >=80 & DHSCLUST <=86)
		replace muni ="PASIG" if DHSYEAR == 2008 & DHSCLUST == 61 | DHSCLUST == 63 | DHSCLUST == 64
		replace province = "METRO MANILA" if DHSYEAR == 2008 & (DHSCLUST >=120 & DHSCLUST <=122)
		replace muni = "LAS PINAS" if DHSYEAR == 2008 & DHSCLUST >=120 & DHSCLUST <=122
		replace muni = "PASIG" if DHSYEAR == 2003 & DHSCLUST == 642
		replace prv = 74 	if (muni == "MANDALUYONG" | muni == "MARIKINA" | muni == "PASIG" | muni == "QUEZON CITY" | muni == "SAN JUAN") & regn == 13 
		replace prv = 74 	if DHSYEAR == 2008 & (DHSCLUST == 28 | DHSCLUST == 34 | DHSCLUST == 41)
		
	// Third District = Caloocan, Malabon, Navotas, Valenzuela
		replace province = "METRO MANILA" if DHSYEAR == 2008 & (DHSCLUST >=98 & DHSCLUST <=104)
		replace muni = "VALENZUELA" if DHSYEAR == 2008 & (DHSCLUST >=98 & DHSCLUST <=104)
		replace province = "METRO MANILA" if DHSYEAR == 20003 & (DHSCLUST >=674 & DHSCLUST <=676)
		replace muni = "VALENZUELA" if DHSYEAR == 2003 & (DHSCLUST >=674 & DHSCLUST <=676)
		replace muni = "MALABON" if DHSYEAR == 2008 & DHSCLUST == 88 
		replace muni = "MALABON" if DHSYEAR == 2003 & DHSCLUST == 662
		replace province = "METRO MANILA" if DHSYEAR == 2003 & DHSCLUST == 662
		replace muni = "MUNTINLUPA" if DHSYEAR == 2003 & DHSCLUST == 706
		replace prv = 75  	if (muni == "CALOOCAN" | muni == "MALABON" | muni =="CITY OF MALABON" | muni == "NAVOTAS" | muni == "CITY OF NAVOTAS" | muni == "VALENZUELA") & regn ==13
		
	// Fourth District = Las Pinas, Makati, Muntinlupa, Paranaque, Pasay, Pateros, Taguig
		replace province = "METRO MANILA" if DHSYEAR == 2008 & (DHSCLUST >=125 & DHSCLUST <=140)
		replace muni = "PARAñAQUE" if DHSYEAR == 2008 & (DHSCLUST >=125 & DHSCLUST <=140)
		replace province = "METRO MANILA" if DHSYEAR == 2008 & (DHSCLUST >=105 & DHSCLUST <=115)
		replace muni = "MAKATI" if DHSYEAR == 2008 & (DHSCLUST >=105 & DHSCLUST <=111)
		replace muni = "TAGUIG" if DHSYEAR == 2008 & DHSCLUST ==115
		replace prv = 76 	if (muni == "LAS PINAS" | muni == "MAKATI" | muni == "MUNTINLUPA" | muni == "PARANAQUE" | muni =="PARAñAQUE" | muni == "PASAY" | muni == "PATEROS" | muni == "TAGUIG") & regn ==13
	
	replace province = "IFUGAO" if DHSYEAR == 2008 & DHSCLUST == 160
	replace muni = "LAGAWE" if DHSYEAR == 2008 & DHSCLUST == 160
	replace muni = "LA TRINIDAD" if DHSYEAR == 2003 & DHSCLUST == 736 | DHSCLUST == 738
	replace prv = 1		if province == "ABRA" & regn == 14
	replace prv = 11	if province == "BENGUET" & regn == 14
	replace prv = 27	if province == "IFUGAO" & regn == 14
	replace prv = 32	if province == "KALINGA" & regn == 14
	replace prv = 44	if province == "MOUNTAIN PROVINCE" & regn == 14
	replace prv = 81	if province == "APAYAO" & regn == 14
	
	replace province = "LANAO DEL SUR" if DHSCLUST == 761 & DHSYEAR == 2003
	replace province = "TAWI-TAWI" if DHSCLUST == 787 & DHSYEAR == 2003
	replace muni = "SAPA-SAPA" if DHSCLUST == 787 & DHSYEAR == 2003
	replace province = "LANAO DEL SUR" if DHSCLUST == 733 & DHSYEAR == 2008
	replace prv = 7		if province == "BASILAN" & regn == 15
	replace province = "LANAO DEL SUR" if province == "COTABATO" & muni == "WAO"
	replace prv = 36	if province == "LANAO DEL SUR" & (regn == 15 | regn ==10)
	replace prv = 36 	if province == "COTABATO" & regn == 15
	replace prv = 38	if province == "MAGUINDANAO" & regn == 15
	replace prv = 66	if province == "SULU" & regn == 15
	replace prv = 70	if province == "TAWI-TAWI" & regn == 15
	
	replace province = "DINAGAT ISLANDS" if DHSYEAR == 2008 & DHSCLUST == 780
	replace province = "DINAGAT ISLANDS" if DHSYEAR == 2008 & DHSCLUST == 786
	replace prv = 2		if province == "AGUSAN DEL NORTE" & regn == 16
	replace prv = 3		if province == "AGUSAN DEL SUR" & regn == 16
	replace prv = 67	if province == "SURIGAO DEL NORTE" & regn == 16
	replace prv = 68	if province == "SURIGAO DEL SUR" & regn == 16
	replace prv = 85	if province == "DINAGAT ISLANDS" & regn == 16
	
	replace province = "ORIENTAL MINDORO" if DHSYEAR == 2008 & DHSCLUST == 386
	replace muni = "BACO" if DHSYEAR == 2008 & DHSCLUST == 386
	replace prv = 40	if province == "MARINDUQUE" & regn == 17
	replace prv = 51	if province == "OCCIDENTAL MINDORO" & regn == 17
	replace prv = 53	if province == "PALAWAN" & regn == 17
	replace prv = 59	if province == "ROMBLON" & regn == 17
	replace province = "ORIENTAL MINDORO" if regn == 17 & (muni == "BACO" | muni == "BANSUD" | muni =="BONGABONG" | muni == "ROXAS" | muni == "POLA" | muni == "VICTORIA")
	replace province = "ORIENTAL MINDORO" if regn == 17 & (muni == "PINAMALAYAN" | muni =="BULALACAO" | muni== "CALAPAN" | muni =="GLORIA" | muni =="PUERTO GALERA" | muni == "PINAMALAYAN")
	replace prv = 52	if province == "ORIENTAL MINDORO" & regn == 17
	
	** Make DHS conform to POEA geocodes (MUNICIPALITY)
	gen mun = 2 		if muni == "BUCARRA" & regn == 1 & prv == 28
	replace muni = "BACARRA" if muni == "" & regn == 1 & prv ==28 & DHSCLUST == 172 & DHSYEAR == 2008
	replace mun = 2 	if muni == "BACARRA" & regn == 1 & prv == 28
	replace mun = 6 	if muni == "BURGOS" & regn == 1 & prv == 28
	replace mun = 19 	if muni == "PINILI" & regn == 1 & prv == 28
	replace mun = 9 	if muni == "DINGRAS" & regn == 1 & prv == 28
	replace mun = 5 	if muni == "BATAC" & regn == 1 & prv == 28
	replace mun = 13		if muni == "MARCOS" & regn == 1 & prv == 28
	replace mun = 15		if muni == "PAGUDPUD" & regn == 1 & prv == 28
	replace mun = 16		if muni == "PAOAY" & regn == 1 & prv == 28
	replace mun = 17		if muni == "PASUQUIN" & regn == 1 & prv == 28
		
	replace mun = 3 	if muni == "BANTAY" & regn == 1 & prv == 29
	replace mun = 6 	if muni == "CANDON" & regn == 1 & prv == 29
	replace mun = 15 	if muni == "QUIRINO" & regn == 1 & prv == 29
	replace mun = 16 	if muni == "SALCEDO" & regn == 1 & prv == 29
	replace mun = 23 	if muni == "SANTA CATALINA" & regn == 1 & prv == 29
	replace mun = 14	if muni == "NARVACAN" & regn == 1 & prv == 29
	replace mun = 20	if muni == "SAN JUAN" & regn == 1 & prv == 29
	replace mun = 30	if muni == "SINAIT" & regn == 1 & prv == 29
	
	replace mun = 3 	if muni == "BACNOTAN" & regn == 1 & prv == 33
	replace mun = 6 	if muni == "BANGAR" & regn == 1 & prv == 33
	replace mun = 7 	if muni == "BAUANG" & regn == 1 & prv == 33
	replace mun = 10	if muni == "LUNA" & regn == 1 & prv == 33
	replace mun = 11	if muni == "NAGUILIAN" & regn == 1 & prv == 33
	replace mun = 14	if muni == "SAN FERNANDO" & regn == 1 & prv == 33
	replace mun = 19	if muni == "SUDIPEN" & regn == 1 & prv == 33
	replace mun = 20	if muni == "TUBAO" & regn == 1 & prv == 33
	
	replace mun = 2 	if muni == "AGUILAR" & regn == 1 & prv == 55
	replace mun = 3 	if muni == "ALAMINOS" & regn == 1 & prv == 55
	replace mun = 9 	if muni == "BASISTA" & regn == 1 & prv == 55
	replace mun = 30	if muni == "POSORRUBIO" & regn == 1 & prv == 55
	replace mun = 14	if muni == "BOLINAO" & regn == 1 & prv == 55
	replace mun = 18	if muni == "DAGUPAN" & regn == 1 & prv == 55
	replace mun = 20	if muni == "INFANTA" & regn == 1 & prv == 55
	replace mun = 24	if muni == "MALASIQUI" & regn == 1 & prv == 55
	replace mun = 31	if muni == "ROSALES" & regn == 1 & prv == 55
	replace mun = 32	if muni == "SAN CARLOS CITY" & regn == 1 & prv == 55
	replace mun = 33	if muni == "SAN FABIAN" & regn == 1 & prv == 55
	replace mun = 34	if muni == "SAN JACINTO" & regn == 1 & prv == 55
	replace mun = 36	if muni == "SAN NICOLAS" & regn == 1 & prv == 55
	replace mun = 39	if muni == "SANTA BARBARA" & regn == 1 & prv == 55
	replace mun = 46	if muni == "URDANETA CITY" & regn == 1 & prv == 55
	replace mun = 6		if muni == "ASINGAN" & regn == 1 & prv == 55
	replace mun = 10	if muni == "BAUTISTA" & regn == 1 & prv == 55
	replace mun = 11	if muni == "BAYAMBANG" & regn == 1 & prv == 55
	replace mun = 12	if muni == "BINALONAN" & regn == 1 & prv == 55
	replace mun = 13	if muni == "BINMALEY" & regn == 1 & prv == 55
	replace muni = "CALASIAO" if muni == "" & regn == 1 & prv == 55 & DHSCLUST == 193 & DHSYEAR == 2008
	replace mun = 17	if muni == "CALASIAO" & regn == 1 & prv == 55
	replace mun = 19	if muni == "DASOL" & regn == 1 & prv == 55
	replace mun = 25	if muni == "MANAOAG" & regn == 1 & prv == 55
	replace mun = 26	if muni == "MANGALDAN" & regn == 1 & prv == 55
	replace mun = 28	if muni == "MAPANDAN" & regn == 1 & prv == 55
	replace mun = 35	if muni == "SAN MANUEL" & regn == 1 & prv == 55
	replace mun = 43	if muni == "TAYUG" & regn == 1 & prv == 55
	replace muni = "ALCALA" if muni == "" & regn == 1 & prv == 55 & DHSCLUST == 207 & DHSYEAR == 2008
	replace mun = 4		if muni == "ALCALA" & regn == 1 & prv == 55
	replace mun = 30	if muni == "POZZORUBIO" & regn == 1 & prv == 55
	replace mun = 30	if muni == "POZORRUBIO" & regn == 1 & prv == 55

	replace mun = 2		if muni == "ITBAYAT" & regn == 2 & prv == 9
	
	replace mun = 1		if muni == "ABULUG" & regn == 2 & prv == 15
	replace mun = 3		if muni == "ALLACAPAN" & regn == 2 & prv == 15
	replace mun = 4		if muni == "AMULUNG" & regn == 2 & prv == 15
	replace mun = 10	if muni == "CAMALANIUGAN" & regn == 2 & prv == 15
	replace mun = 11	if muni == "CLAVERIA" & regn == 2 & prv == 15
	replace mun = 12	if muni == "ENRILE" & regn == 2 & prv == 15
	replace mun = 13	if muni == "GATTARAN" & regn == 2 & prv == 15
	replace mun = 14	if muni == "GONZAGA" & regn == 2 & prv == 15
	replace mun = 15	if muni == "IGUIG" & regn == 2 & prv == 15
	replace mun = 16	if muni == "LAL-LO" & regn == 2 & prv == 15
	replace mun = 18	if muni == "PAMPLONA" & regn == 2 & prv == 15
	replace mun = 29	if muni == "TUGUEGARAO" & regn == 2 & prv == 15
	replace mun = 2		if muni == "ALCALA" & regn == 2 & prv == 15
	replace mun = 6		if muni == "BAGGAO" & regn == 2 & prv == 15
	replace mun = 22	if muni == "SANCHEZ MIRA" & regn == 2 & prv == 15
	replace mun = 26	if muni == "SANTO NIñO" & regn == 2 & prv == 15
	replace mun = 27	if muni == "SOLANA" & regn == 2 & prv == 15
	replace muni = "SAN MATEO" if DHSYEAR == 2003 & (DHSCLUST == 64 |DHSCLUST == 63)
	replace mun = 1		if muni == "ALICIA" & regn == 2 & prv == 31
	replace mun = 5 	if muni == "BURGOS" & regn == 2 & prv == 31
	replace mun = 8		if muni == "CAUAYAN CITY" & regn == 2 & prv == 31
	replace mun = 12	if muni == "ECHAGUE" & regn == 2 & prv == 31
	replace mun = 14	if muni == "ILAGAN CITY" & regn == 2 & prv == 31
	replace mun = 25	if muni == "REINA MERCEDES" & regn == 2 & prv == 31
	replace mun = 26	if muni == "ROXAS" & regn == 2 & prv == 31
	replace mun = 35	if muni == "SANTIAGO" & regn == 2 & prv == 31
	replace mun = 31	if muni == "SAN MARIANO" & regn == 2 & prv == 31
	replace mun = 7		if muni == "CABATUAN" & regn == 2 & prv == 31
	replace mun = 4		if muni == "BENITO SOLIVEN" & regn == 2 & prv == 31
	replace mun = 9		if muni == "CORDON" & regn == 2 & prv == 31
	replace mun = 15	if muni == "JONES" & regn == 2 & prv == 31
	replace mun = 19	if muni == "MALLIG" & regn == 2 & prv == 31
	replace mun = 22	if muni == "QUEZON" & regn == 2 & prv == 31
	replace muni = "SAN MANUEL" if DHSYEAR == 2008 & DHSCLUST == 231
	replace mun = 30	if muni == "SAN MANUEL" & regn == 2 & prv == 31
	replace mun = 32	if muni == "SAN MATEO" & regn == 2 & prv == 31
	replace mun = 34	if muni == "SANTA MARIA" & regn == 2 & prv == 31
	
	replace muni = "BAGABAG" if DHSCLUST == 235 & DHSYEAR == 2008
	replace mun = 3 	if muni == "BAGABAG" & regn == 2 & prv == 50
	replace mun = 5		if muni == "BAYOMBONG" & regn == 2 & prv == 50
	replace mun = 6		if muni == "DIADI" & regn == 2 & prv == 50
	replace mun = 13	if muni == "SOLANO" & regn == 2 & prv == 50
	replace mun = 4		if muni == "BAMBANG" & regn == 2 & prv == 50
	replace mun = 11	if muni == "QUEZON" & regn == 2 & prv == 50

	replace mun = 3		if muni == "DIFFUN" & regn == 2 & prv == 57
	replace mun = 4		if muni == "MADDELA" & regn == 2 & prv == 57
	replace mun = 2		if muni == "CABARROGUIS" & regn == 2 & prv == 57

	replace mun = 4		if muni == "DINALUPIHAN" & regn == 3 & prv == 8
	replace mun = 7		if muni == "MARIVELES" & regn == 3 & prv == 8
	replace mun = 1		if muni == "ABUCAY" & regn == 3 & prv == 8
	replace mun = 10	if muni == "ORION" & regn == 3 & prv == 8
	replace mun = 8		if muni == "MORONG" & regn == 3 & prv == 8

	replace mun = 6		if muni == "BUSTOS" & regn == 3 & prv == 14
	replace mun = 8		if muni == "GUIGUINTO" & regn == 3 & prv == 14	
	replace muni = "MALOLOS" if muni == "" & DHSCLUST == 249 & DHSYEAR == 2008
	replace mun = 10	if muni == "MALOLOS" & regn == 3 & prv == 14
	replace mun = 9		if muni == "HAGONOY" & regn == 3 & prv == 14
	replace muni = "MARILAO" if DHSYEAR == 2008 & (DHSCLUST == 251 | DHSCLUST == 252 | DHSCLUST == 253 | DHSCLUST ==254)
	replace mun = 11	if muni == "MARILAO" & regn == 3 & prv == 14
	replace muni = "MEYCAUAYAN" if DHSYEAR == 2003 & DHSCLUST == 82
	replace mun = 12	if muni == "MEYCAUAYAN" & regn == 3 & prv == 14
	replace mun = 13	if muni == "NORAGARAY" & regn == 3 & prv == 14
	replace mun = 18	if muni == "PULILAN" & regn == 3 & prv == 14
	replace mun = 20	if muni == "SAN JOSE DEL MONTE CITY" & regn == 3 & prv == 14
	replace mun = 23	if muni == "SANTA MARIA" & regn == 3 & prv == 14
	replace mun = 14	if muni == "OBONDO" & regn == 3 & prv == 14
	replace mun = 14	if muni == "OBANDO" & regn == 3 & prv == 14
	replace mun = 15	if muni == "PANDI" & regn == 3 & prv == 14
	replace mun = 21	if muni == "SAN MIGUEL" & regn == 3 & prv == 14
	replace mun = 5 	if muni == "BULACAN" & regn == 3 & prv == 14
	replace mun = 13 	if muni == "NORZAGARAY" & regn == 3 & prv ==14
	replace mun = 7 	if muni == "CALUMPIT" & regn == 3 & prv == 14
	
	replace mun = 2		if muni == "BONGABON" & regn == 3 & prv == 49
	replace mun = 3		if muni == "CABANATUAN CITY" & regn == 3 & prv == 49
	replace mun = 4		if muni == "CABIAO" & regn == 3 & prv == 49
	replace mun = 5		if muni == "CARRANGLAN" & regn == 3 & prv == 49
	replace mun = 9		if muni == "GENERAL MAMERTO NATIVIDAD" & regn == 3 & prv == 49
	replace mun = 17	if muni == "SCIENCE CITY OF MUNOZ" & regn == 3 & prv == 49
	replace mun = 25	if muni == "SAN ISIDRO" & regn == 3 & prv == 49
	replace mun = 26	if muni == "SAN JOSE CITY" & regn == 3 & prv == 49
	replace mun = 27	if muni == "SAN LEONARDO" & regn == 3 & prv == 49
	replace mun = 30	if muni == "TALAVERA" & regn == 3 & prv == 49
	replace mun = 1		if muni == "ALIAGA" & regn == 3 & prv == 49
	replace mun = 6		if muni == "CUYAPO" & regn == 3 & prv == 49
	replace mun = 7		if muni == "GABALDON" & regn == 3 & prv == 49
	replace mun = 8		if muni == "GAPAN CITY" & regn == 3 & prv == 49
	replace mun = 12	if muni == "JAEN" & regn == 3 & prv == 49
	replace mun = 19	if muni == "PALAYAN CITY" & regn == 3 & prv == 49
	replace mun = 21	if muni == "PENARANDA" & regn == 3 & prv == 49
	
	replace muni = "APALIT" if DHSCLUST == 109 & DHSYEAR == 2003
	replace mun = 1		if muni == "ANGELES" & regn == 3 & prv == 54
	replace mun = 6		if muni == "FLORIDABLANCA" & regn == 3 & prv == 54
	replace mun = 14	if muni == "MINALIN" & regn == 3 & prv == 54
	replace mun = 7		if muni == "GUAGUA" & regn == 3 & prv == 54
	replace mun = 22	if muni == "SASMUAN" & regn == 3 & prv == 54
	replace mun = 12	if muni == "MASANTOL" & regn == 3 & prv == 54
	replace mun = 13	if muni == "MEXICO" & regn == 3 & prv == 54
	replace mun = 16	if muni == "SAN FERNANDO" & regn == 3 & prv == 54

	replace mun = 2		if muni == "APALIT" & regn == 3 & prv == 54
	replace mun = 8		if muni == "LUBAO" & regn == 3 & prv == 54
	replace mun = 9		if muni == "MABALACAT" & regn == 3 & prv == 54
	replace mun = 17	if muni == "SAN LUIS" & regn == 3 & prv == 54
	replace mun = 18	if muni == "SAN SIMON" & regn == 3 & prv == 54
	replace mun = 10	if muni == "MACABEBE" & regn == 3 & prv == 54
		
	replace mun = 4		if muni == "CAPAS" & regn == 3 & prv == 69
	replace mun = 3		if muni == "CAMILING" & regn == 3 & prv == 69
	replace mun = 9		if muni == "MONCADO" & regn == 3 & prv == 69
	replace muni = "TARLAC CITY" if DHSCLUST == 293 & DHSYEAR == 2008
	replace mun = 16	if muni == "TARLAC CITY" & regn == 3 & prv == 69
	replace mun = 1		if muni == "ANAO" & regn == 3 & prv == 69
	replace mun = 5		if muni == "CONCEPCION" & regn == 3 & prv == 69
	replace mun = 7		if muni == "LA PAZ" & regn == 3 & prv == 69
	replace mun = 9		if muni == "MONCADA" & regn == 3 & prv == 69
	replace mun = 11	if muni == "PURA" & regn == 3 & prv == 69
	
	replace mun = 1		if muni == "BOTOLAN" & regn == 3 & prv == 71
	replace mun = 10	if muni == "SAN FELIPE" & regn == 3 & prv == 71
	replace mun = 14	if muni == "SUBIC" & regn == 3 & prv == 71
	replace mun = 7		if muni == "OLONGAPO" & regn == 3 & prv == 71
	replace mun = 2		if muni == "CABANGAN" & regn == 3 & prv == 71
	replace muni = "SANTA CRUZ" if DHSCLUST == 298 & DHSYEAR == 2008
	replace mun = 13	if muni == "SANTA CRUZ" & regn == 3 & prv == 71
	
	replace mun = 7		if muni == "MARIA AURORA" & regn == 3 & prv == 77
	
	replace mun = 25	if muni == "SAN NICOLAS" & regn == 4 & prv == 10
	replace mun = 3		if muni == "BALAYAN" & regn == 4 & prv == 10
	replace mun = 5		if muni == "BATANGAS" & regn == 4 & prv == 10
	replace mun = 6		if muni == "BAUAN" & regn == 4 & prv == 10
	replace mun = 10	if muni == "IBAAN" & regn == 4 & prv == 10
	replace mun = 11	if muni == "LAUREL" & regn == 4 & prv == 10
	replace mun = 14	if muni == "LIPA" & regn == 4 & prv == 10
	replace mun = 15	if muni == "LOBO" & regn == 4 & prv == 10
	replace mun = 17	if muni == "MALVAR" & regn == 4 & prv == 10
	replace mun = 23	if muni == "SAN JUAN" & regn == 4 & prv == 10
	replace mun = 30	if muni == "TALISAY" & regn == 4 & prv == 10
	replace mun = 31	if muni == "TANAUAN" & regn == 4 & prv == 10
	replace mun = 1		if muni == "AGONCILLO" & regn == 4 & prv == 10
	replace mun = 12	if muni == "LEMERY" & regn == 4 & prv == 10
	replace mun = 26	if muni == "SAN PASCUAL" & regn == 4 & prv == 10
	
	replace muni = "BACOOR" if DHSYEAR == 2008 & (DHSCLUST >=311 & DHSCLUST <=316)
	replace mun = 3		if muni == "BACOOR" & regn == 4 & prv == 21
	replace mun = 6		if muni == "DASMARINAS" & regn == 4 & prv == 21
	replace mun = 8		if muni == "GENERAL TRIAS" & regn == 4 & prv == 21
	replace mun = 19	if muni == "TAGAYTAY" & regn == 4 & prv == 21
	replace mun = 20	if muni == "TANZA" & regn == 4 & prv == 21
	replace mun = 4		if muni == "CARMONA" & regn == 4 & prv == 21
	replace mun = 5		if muni == "CAVITE CITY" & regn == 4 & prv == 21
	replace mun = 6		if muni == "DASMARIñAS" & regn == 4 & prv == 21
	replace mun = 9		if muni == "IMUS" & regn == 4 & prv == 21
	replace mun = 11	if muni == "KAWIT" & regn == 4 & prv == 21
	replace mun = 17	if muni == "ROSARIO" & regn == 4 & prv == 21
	replace muni = "SILANG" if DHSCLUST == 328 & DHSYEAR == 2008
	replace mun = 18	if muni == "SILANG" & regn == 4 & prv == 21
	replace muni = "MARAGONDON" if DHSCLUST == 325 & DHSYEAR == 2008
	replace mun = 13 	if muni == "MARAGONDON" & regn == 4 & prv == 21
	replace muni = "NAIC" if muni =="NAITE" & regn == 4 & prv == 21
	replace mun = 15	if muni == "NAIC" & regn == 4 & prv ==21
	
	replace muni = "SAN PEDRO" if DHSYEAR == 2008 & DHSCLUST == 344
	replace mun = 3		if muni == "BINAN" & regn == 4 & prv == 34
	replace mun = 5		if muni == "CALAMBA" & regn == 4 & prv == 34
	replace mun = 13	if muni == "LUMBAN" & regn == 4 & prv == 34
	replace mun = 15	if muni == "MAGDALENA" & regn == 4 & prv == 34
	replace mun = 19	if muni == "PAGSANJAN" & regn == 4 & prv == 34
	replace mun = 20	if muni == "PAKIL" & regn == 4 & prv == 34
	replace mun = 22	if muni == "PILA" & regn == 4 & prv == 34
	replace mun = 24	if muni == "SAN PABLO CITY" & regn == 4 & prv == 34
	replace mun = 26	if muni == "SANTA CRUZ" & regn == 4 & prv == 34
	replace mun = 28	if muni == "SANTA ROSA" & regn == 4 & prv == 34
	replace mun = 1		if muni == "ALAMINOS" & regn == 4 & prv == 34
	replace mun = 3		if muni == "BIñAN" & regn == 4 & prv == 34
	replace mun = 16	if muni == "MAJAYJAY" & regn == 4 & prv == 34
	replace mun = 17	if muni == "NAGCARLAN" & regn == 4 & prv == 34
	replace mun = 18	if muni == "PAETE" & regn == 4 & prv == 34
	replace mun = 23	if muni == "RIZAL" & regn == 4 & prv == 34
	replace mun = 25	if muni == "SAN PEDRO" & regn ==4 & prv == 34
	
	replace mun = 3		if muni == "ATIMONAN" & regn == 4 & prv == 56
	replace mun = 8		if muni == "CANDELARIA" & regn == 4 & prv == 56
	replace mun = 19	if muni == "GUMACA" & regn == 4 & prv == 56
	replace mun = 24	if muni == "LUCENA" & regn == 4 & prv == 56
	replace mun = 30	if muni == "PAGBILAO" & regn == 4 & prv == 56
	replace mun = 34	if muni == "PITOGO" & regn == 4 & prv == 56
	replace mun = 25	if muni == "MACALELON" & regn == 4 & prv == 56
	replace mun = 41	if muni == "SAN ANTONIO" & regn == 4 & prv == 56
	replace mun = 42	if muni == "SAN FRANCISCO" & regn == 4 & prv == 56
	replace mun = 45	if muni == "SARIAYA" & regn == 4 & prv == 56
	replace mun = 48	if muni == "TIAONG" & regn == 4 & prv == 56
	replace mun = 10	if muni == "CATANAUAN" & regn == 4 & prv == 56
	replace mun = 38	if muni == "REAL" & regn == 4 & prv == 56
	replace muni = "CALAUAG" if DHSCLUST == 355 & DHSYEAR == 2008
	replace mun = 7 	if muni == "CALAUAG" & regn == 4 & prv == 56
	replace muni = "TAGKAWAYAN" if DHSCLUST ==348 & DHSYEAR == 2008
	replace mun = 46 	if muni =="TAGKAWAYAN" & regn == 4 & prv == 56
	
	replace muni = "ANTIPOLO" if DHSYEAR == 2008 & DHSCLUST == 366
	replace mun = 2		if muni == "ANTIPOLO" & regn == 4 & prv == 58
	replace mun = 12	if muni == "TANAY" & regn == 4 & prv == 58
	replace mun = 4		if muni == "BINANGONAN" & regn == 4 & prv == 58
	replace mun = 13	if muni == "TAYTAY" & regn == 4 & prv == 58
	replace mun = 5		if muni == "CAINTA" & regn == 4 & prv == 58
	replace mun = 6		if muni == "CARDONA" & regn == 4 & prv == 58
	replace mun = 7		if muni == "JALA-JALA" & regn == 4 & prv == 58
	replace mun = 8		if muni == "RODRIGUEZ" & regn == 4 & prv == 58
	replace mun = 9		if muni == "MORONG" & regn == 4 & prv == 58
	replace mun = 10	if muni == "PILILLA" & regn == 4 & prv == 58
	replace mun = 11	if muni == "SAN MATEO" & regn == 4 & prv == 58
	
	replace muni = "PALANAS" if DHSYEAR ==2003 & DHSCLUST == 270
	replace mun = 1		if muni == "BACACAY" & regn == 5 & prv == 5
	replace mun = 2		if muni == "CAMALIG" & regn == 5 & prv == 5
	replace mun = 3		if muni == "DARAGA" & regn == 5 & prv == 5
	replace mun = 6		if muni == "LEGAZPI CITY" & regn == 5 & prv == 5
	replace mun = 7		if muni == "LIBON" & regn == 5 & prv == 5
	replace mun = 18	if muni == "TIWI" & regn == 5 & prv == 5
	replace mun = 11	if muni == "MANITO" & regn == 5 & prv == 5
	replace mun = 8		if muni == "LIGAO" & regn == 5 & prv == 5
	replace mun = 4		if muni == "GUINOBATAN" & regn == 5 & prv == 5
	replace mun = 12	if muni == "OAS" & regn == 5 & prv == 5
	replace mun = 16	if muni == "SANTO DOMINGO" & regn == 5 & prv == 5
	
	replace muni = "VINZONS" if DHSYEAR == 2008 & DHSCLUST == 419
	replace muni = "SANTA ELENA" if DHSYEAR == 2008 & DHSCLUST == 418
	replace mun = 3		if muni == "DAET" & regn == 5 & prv == 16
	replace mun = 5		if muni == "JOSE PANGANIBAN" & regn == 5 & prv == 16
	replace mun = 4		if muni == "SAN LORENZO RUIZ" & regn == 5 & prv == 16
	replace mun = 10	if muni == "SANTA ELENA" & regn == 5 & prv == 16
	replace mun = 1		if muni == "BASUD" & regn == 5 & prv == 16
	replace mun = 12	if muni == "VINZONS" & regn == 5 & prv == 16
	
	replace mun = 3		if muni == "POBLACION, BATO" & regn == 5 & prv == 17
	replace mun = 6		if muni == "BULA" & regn == 5 & prv == 17
	replace mun = 8		if muni == "CALABANGA" & regn == 5 & prv == 17
	replace mun = 24	if muni == "NAGA" & regn == 5 & prv == 17
	replace mun = 7		if muni == "CABUSAO" & regn == 5 & prv == 17
	replace mun = 16	if muni == "CITY OF IRIGA" & regn == 5 & prv == 17
	replace mun = 32	if muni == "SAN FERNANDO" & regn == 5 & prv == 17
	replace mun = 3		if muni == "BATO" & regn == 5 & prv == 17
	replace mun = 27	if muni == "PASACAO" & regn == 5 & prv == 17
	replace mun = 1		if muni == "BAAO" & regn == 5 & prv == 17
	replace mun = 9		if muni == "CAMALIGAN" & regn == 5 & prv == 17
	replace mun = 10	if muni == "CANAMAN" & regn == 5 & prv == 17
	replace mun = 18	if muni == "LIBMANAN" & regn == 5 & prv == 17
	replace mun = 21	if muni == "MILAOR" & regn == 5 & prv == 17
	replace mun = 34	if muni == "SIPOCOT" & regn == 5 & prv == 17

	replace mun = 2		if muni == "BARAS" & regn == 5 & prv == 20
	replace mun = 11	if muni == "VIRAC" & regn == 5 & prv == 20
	replace mun = 4		if muni == "CARAMORAN" & regn == 5 & prv == 20
	replace mun = 6		if muni == "PANDAN" & regn == 5 & prv == 20

	replace muni = "MILAGROS" if DHSYEAR == 2008 & DHSCLUST == 436
	replace mun = 1		if muni == "AROROY" & regn == 5 & prv == 41
	replace mun = 6		if muni == "CAWAYAN" & regn == 5 & prv == 41
	replace mun = 12	if muni == "MILAGROS" & regn == 5 & prv == 41
	replace mun = 16	if muni == "PIO V. CORPUS" & regn == 5 & prv == 41
	replace mun = 17	if muni == "PLACER" & regn == 5 & prv == 41
	replace mun = 3		if muni == "BALUD" & regn == 5 & prv == 41
	replace mun = 4		if muni == "BATUAN" & regn == 5 & prv == 41
	replace mun = 19	if muni == "SAN JACINTO" & regn == 5 & prv == 41
	replace mun = 21	if muni == "USON" & regn == 5 & prv == 41
	replace mun = 15	if muni == "PALANAS" & regn == 5 & prv == 41
	replace mun = 12 	if muni == "MILAGROS" & regn == 5 & prv == 41
	
	replace muni = "SORSOGON CITY" if DHSCLUST == 444 & DHSYEAR == 2008
	replace mun = 2		if muni == "BARCELONA" & regn == 5 & prv == 62
	replace mun = 5		if muni == "CASIGURAN" & regn == 5 & prv == 62
	replace mun = 16	if muni == "SORSOGON CITY" & regn == 5 & prv == 62
	replace mun = 12	if muni == "MATNOG" & regn == 5 & prv == 62
	replace mun = 13	if muni == "PILAR" & regn == 5 & prv == 62
	replace mun = 3		if muni == "BULAN" & regn == 5 & prv == 62
	replace mun = 6		if muni == "CASTILLA" & regn == 5 & prv == 62
	replace mun = 15	if muni == "SANTA MAGDALENA" & regn == 5 & prv == 62
	
	replace mun = 4		if muni == "BATAN" & regn == 6 & prv == 4
	replace mun = 14	if muni == "NABAS" & regn == 6 & prv == 4
	replace mun = 17	if muni == "TANGALAN" & regn == 6 & prv == 4
	replace mun = 7		if muni == "KALIBO" & regn == 6 & prv == 4
	replace mun = 15	if muni == "NEW WASHINGTON" & regn == 6 & prv == 4
	
	replace mun = 10	if muni == "LIBERTAD" & regn == 6 & prv == 6
	replace mun = 13	if muni == "SAN JOSE DE BUENAVISTA" & regn == 6 & prv == 6
	replace mun = 8		if muni == "HAMTIC" & regn == 6 & prv == 6
	replace mun = 1		if muni == "ANINI-Y" & regn == 6 & prv == 6
	replace mun = 4		if muni == "BUGASONG" & regn == 6 & prv == 6
	replace mun = 6		if muni == "CULASI" & regn == 6 & prv == 6
	
	replace mun = 5		if muni == "IVISAN" & regn == 6 & prv == 19
	replace mun = 6		if muni == "JAMINDAN" & regn == 6 & prv == 19
	replace mun = 10	if muni == "PANITAN" & regn == 6 & prv == 19
	replace mun = 9		if muni == "PANAY" & regn == 6 & prv == 19
	replace mun = 14	if muni == "ROXAS CITY" & regn == 6 & prv == 19
	replace mun = 2		if muni == "DAO" & regn == 6 & prv == 19
	replace mun = 8		if muni == "MAMBUSAO" & regn == 6 & prv == 19

	replace muni = "BATAD" if DHSCLUST == 459 & DHSYEAR == 2008
	replace muni = "JANIUAY" if DHSCLUST == 465 & DHSYEAR == 2008
	replace mun = 14	if muni == "CARLES" & regn == 6 & prv == 30
	replace mun = 16	if muni == "DINGLE" & regn == 6 & prv == 30
	replace mun = 3		if muni == "ANILAO" & regn == 6 & prv == 30
	replace mun = 21	if muni == "IGBARAS" & regn == 6 & prv == 30
	replace mun = 22	if muni == "ILOILO CITY" & regn == 6 & prv == 30
	replace mun = 35	if muni == "PASSI CITY" & regn == 6 & prv == 30
	replace mun = 39	if muni == "SAN ENRIQUE" & regn == 6 & prv == 30
	replace mun = 18	if muni == "DUMANGAS" & regn == 6 & prv == 30
	replace mun = 47	if muni == "ZARRAGA" & regn == 6 & prv == 30
	replace mun = 26	if muni == "LEGANES CITY" & regn == 6 & prv == 30
	replace mun = 45	if muni == "TIGBAUAN" & regn == 6 & prv == 30
	replace mun = 7		if muni == "BAROTAC NUEVO" & regn == 6 & prv == 30
	replace mun = 8		if muni == "BAROTAC VIEJO" & regn == 6 & prv == 30
	replace mun = 9		if muni == "BATAD" & regn == 6 & prv == 30
	replace mun = 15	if muni == "CONCEPCION" & regn == 6 & prv == 30
	replace mun = 23	if muni == "JANIUAY" & regn == 6 & prv == 30
	replace mun = 28	if muni == "LEON" & regn == 6 & prv == 30
	replace mun = 29	if muni == "MAASIN" & regn == 6 & prv == 30
	replace mun = 30 	if muni == "MIAGAO" & regn == 6 & prv == 30
	replace mun = 36	if muni == "PAVIA" & regn == 6 & prv == 30
	replace mun = 40	if muni == "SAN JOAQUIN" & regn == 6 & prv == 30
	
	replace mun = 1		if muni == "BACOLOD" & regn == 6 & prv == 45
	replace mun = 2		if muni == "BAGO CITY" & regn == 6 & prv == 45
	replace mun = 7		if muni == "CAUAYAN" & regn == 6 & prv == 45
	replace mun = 8		if muni == "ENRIQUE B. MAGALONA" & regn == 6 & prv == 45
	replace mun = 9		if muni == "ESCALANTE CITY" & regn == 6 & prv == 45
	replace mun = 14	if muni == "ISABELA" & regn == 6 & prv == 45
	replace mun = 15	if muni == "KABANKALAN" & regn == 6 & prv == 45
	replace mun = 30	if muni == "VALLADOLID" & regn == 6 & prv == 45
	replace mun = 24	if muni == "SAN CARLOS CITY" & regn == 6 & prv == 45
	replace mun = 25	if muni == "SAN ENRIQUE" & regn == 6 & prv == 45
	replace mun = 26	if muni == "SILAY CITY" & regn == 6 & prv == 45
	replace mun = 28	if muni == "TALISAY CITY" & regn == 6 & prv == 45
	replace mun = 32	if muni == "DON SALVADOR BENEDICTO" & regn == 6 & prv == 45
	replace mun = 4		if muni == "CADIZ CITY" & regn == 6 & prv == 45
	replace mun = 6		if muni == "CANDONI" & regn == 6 & prv == 45
	replace mun = 10	if muni == "HIMAMAYLAN CITY" & regn == 6 & prv == 45
	replace mun = 21	if muni == "PONTEVEDRA" & regn == 6 & prv == 45
	replace mun = 23	if muni == "SAGAY CITY" & regn == 6 & prv == 45
	replace mun = 29	if muni == "TOBOSO" & regn == 6 & prv == 45
	
	replace mun = 3		if muni == "NUEVA VALENCIA" & regn == 6 & prv == 79
	replace mun = 1		if muni == "BUENAVISTA" & regn == 6 & prv == 79
	
	replace mun = 9		if muni == "BUENAVISTA" & regn == 7 & prv == 12
	replace mun = 23	if muni == "GUINDULMAN" & regn == 7 & prv == 12
	replace mun = 37	if muni == "SAN ISIDRO" & regn == 7 & prv == 12
	replace mun = 38	if muni == "SAN MIGUEL" & regn == 7 & prv == 12
	replace mun = 6		if muni == "BALILIHAN" & regn == 7 & prv == 12
	replace mun = 45	if muni == "TUBIGON" & regn == 7 & prv == 12
	replace mun = 48	if muni == "BIEN UNIDO" & regn == 7 & prv == 12
	replace mun = 14	if muni == "CLARIN" & regn == 7 & prv == 12
	replace mun = 24	if muni == "INABANGA" & regn == 7 & prv == 12
	replace mun = 25	if muni == "JAGNA" & regn == 7 & prv == 12
	replace mun = 28	if muni == "LOAY" & regn == 7 & prv == 12
	replace mun = 31	if muni == "MABINI" & regn == 7 & prv == 12
	replace mun = 33	if muni == "PANGLAO" & regn == 7 & prv == 12
	replace mun = 42	if muni == "TAGBILARAN CITY" & regn == 7 & prv == 12
	
	replace muni = "SAN FERNANDO" if DHSYEAR == 2008 & DHSCLUST == 524
	replace muni = "ALEGRIA" if muni == "ALEGRíA" & regn == 7 & prv ==22
	replace muni = "BALAMBAN" if DHSYEAR == 2003 & DHSCLUST == 338
	replace mun = 22	if muni == "DALAGUETE" & regn == 7 & prv == 22
	replace mun = 5		if muni == "ARGAO" & regn == 7 & prv == 22
	replace mun = 8		if muni == "BALAMBAN" & regn == 7 & prv == 22
	replace mun = 14	if muni == "CARCAR CITY" & regn == 7 & prv == 22
	replace mun = 17	if muni == "CEBU CITY" & regn == 7 & prv == 22
	replace mun = 30	if muni == "MANDAUE CITY" & regn == 7 & prv == 22
	replace mun = 20	if muni == "CORDOVA" & regn == 7 & prv == 22
	replace mun = 3		if muni == "ALEGRIA" & regn == 7 & prv == 22
	replace mun = 26	if muni == "LAPU-LAPU CITY" & regn == 7 & prv == 22
	replace mun = 19	if muni == "CONSOLACION" & regn == 7 & prv == 22
	replace mun = 29	if muni == "MALABUYOC" & regn == 7 & prv == 22
	replace mun = 33	if muni == "MOALBOAL" & regn == 7 & prv == 22
	replace mun = 39	if muni == "RONDA" & regn == 7 & prv == 22
	replace mun = 50	if muni == "TALISAY CITY" & regn == 7 & prv == 22
	replace mun = 51	if muni == "TOLEDO CITY" & regn == 7 & prv == 22
	replace mun = 52	if muni == "TUBURAN" & regn == 7 & prv == 22
	replace mun = 6		if muni == "ASTURIAS" & regn == 7 & prv == 22
	replace mun = 18	if muni == "COMPOSTELA" & regn == 7 & prv == 22
	replace mun = 32	if muni == "MINGLANILLA" & regn == 7 & prv == 22
	replace mun = 41	if muni == "SAN FERNANDO" & regn == 7 & prv == 22
	replace mun = 43	if muni == "SAN REMIGIO" & regn == 7 & prv == 22
	
	replace muni = "BAIS" if DHSYEAR == 2008 & DHSCLUST == 530
	replace muni = "MABINAY" if DHSYEAR == 2008 & DHSCLUST == 531
	replace muni = "SIBULAN" if DHSYEAR == 2008 & DHSCLUST == 536
	replace mun = 7		if muni == "BINDOY" & regn == 7 & prv == 46
	replace mun = 10	if muni == "DUMAGUETE" & regn == 7 & prv == 46
	replace mun = 11	if muni == "GUIHULNGAN CITY" & regn == 7 & prv == 46
	replace mun = 22	if muni == "TAYASAN" & regn == 7 & prv == 46
	replace mun = 17	if muni == "SAN JOSE" & regn == 7 & prv == 46
	replace mun = 18	if muni == "SANTA CATALINA" & regn == 7 & prv == 46
	replace mun = 19	if muni == "SIATON" & regn == 7 & prv == 46
	replace mun = 21	if muni == "TANJAY CITY" & regn == 7 & prv == 46
	replace mun = 24	if muni == "VALLEHERMOSO" & regn == 7 & prv == 46
	replace mun = 12	if muni == "JIMALALUD" & regn == 7 & prv == 46
	replace mun = 14	if muni == "MABINAY" & regn == 7 & prv == 46
	replace mun = 23	if muni == "VALENCIA" & regn == 7 & prv == 46
	replace mun = 4		if muni == "BAIS" & regn == 7 & prv == 46
	replace mun = 20 	if muni == "SIBULAN" & regn == 7 & prv == 46
	
	replace muni = "SIQUIJOR" if muni == "" & province == "SIQUIJOR" & DHSYEAR == 2003
	replace mun = 6 	if muni == "SIQUIJOR" & regn == 7 & prv == 61
	replace mun = 3		if muni == "LAZI" & regn == 7 & prv == 61
	
	replace muni = "CAN-AVID" if DHSCLUST == 540 & DHSYEAR == 2008
	replace mun = 15	if muni == "MAYDOLONG" & regn == 8 & prv == 26
	replace mun = 9		if muni == "GUIUAN" & regn == 8 & prv == 26
	replace mun = 10	if muni == "HERNANI" & regn == 8 & prv == 26
	replace mun = 21	if muni == "SAN POLICARPO" & regn == 8 & prv == 26
	replace mun = 22	if muni == "SULAT" & regn == 8 & prv == 26
	replace mun = 23	if muni == "TAFT" & regn == 8 & prv == 26
	replace mun = 5		if muni == "CAN-AVID" & regn == 8 & prv == 26
	
	replace muni = "HILONGOS" if DHSCLUST == 547 & DHSYEAR == 2008
	replace muni = "CAPOOCAN" if DHSCLUST == 551 & DHSYEAR == 2008
	replace muni = "MATALOM" if DHSCLUST == 387 & DHSYEAR == 2003
	replace mun = 1		if muni == "ABUYOG" & regn == 8 & prv == 37
	replace mun = 5		if muni == "BABATNGON" & regn == 8 & prv == 37
	replace mun = 6		if muni == "BARUGO" & regn == 8 & prv == 37
	replace mun = 7		if muni == "BATO" & regn == 8 & prv == 37
	replace mun = 8		if muni == "BAYBAY CITY" & regn == 8 & prv == 37
	replace mun = 13	if muni == "CALUBIAN" & regn == 8 & prv == 37
	replace mun = 19	if muni == "HILONGOS" & regn == 8 & prv == 37
	replace mun = 24	if muni == "JAVIER" & regn == 8 & prv == 37
	replace mun = 34	if muni == "MATALOM" & regn == 8 & prv == 37
	replace mun = 38	if muni == "ORMOC" & regn == 8 & prv == 37
	replace mun = 42	if muni == "SAN ISIDRO" & regn == 8 & prv == 37
	replace mun = 43	if muni == "SAN MIGUEL" & regn == 8 & prv == 37
	replace mun = 47	if muni == "TACLOBAN CITY" & regn == 8 & prv == 37
	replace mun = 15	if muni == "CARIGARA" & regn == 8 & prv == 37
	replace mun = 22	if muni == "ISABEL" & regn == 8 & prv == 37
	replace mun = 23	if muni == "JARO" & regn == 8 & prv == 37
	replace mun = 33	if muni == "MATAG-OB" & regn == 8 & prv == 37
	replace mun = 35	if muni == "MAYORGA" & regn == 8 & prv == 37
	replace mun = 51	if muni == "VILLABA" & regn == 8 & prv == 37
	replace mun = 14	if muni == "CAPOOCAN" & regn == 8 & prv == 37
	
	replace muni = "LAS NAVAS" if DHSCLUST == 561 & DHSYEAR == 2008
	replace mun = 1		if muni == "ALLEN" & regn == 8 & prv == 48
	replace mun = 7		if muni == "GAMAY" & regn == 8 & prv == 48
	replace mun = 11	if muni == "LAVEZARES" & regn == 8 & prv == 48
	replace mun = 5		if muni == "CATARMAN" & regn == 8 & prv == 48
	replace mun = 10	if muni == "LAS NAVAS" & regn == 8 & prv == 48
	replace mun = 16	if muni == "ROSARIO" & regn == 8 & prv == 48
	
	replace mun = 3		if muni == "CALBAYOG CITY" & regn == 8 & prv == 60
	replace mun = 2		if muni == "BASEY" & regn == 8 & prv == 60
	replace mun = 6		if muni == "DARAM" & regn == 8 & prv == 60
	replace mun = 10	if muni == "MARABUT" & regn == 8 & prv == 60
	replace mun = 22	if muni == "PARANAS" & regn == 8 & prv == 60
		
	replace mun = 3		if muni == "HINUNANGAN" & regn == 8 & prv == 64
	replace mun = 5		if muni == "LIBAGON" & regn == 8 & prv == 64
	replace mun = 7		if muni == "MAASIN CITY" & regn == 8 & prv == 64
	replace mun = 2		if muni == "BONTOC" & regn == 8 & prv == 64
	replace mun = 11	if muni == "PINTUYAN" & regn == 8 & prv == 64
	
	replace mun = 3		if muni == "CABUCGAYAN" & regn == 8 & prv == 78
	replace mun = 4		if muni == "CAIBIRAN" & regn == 8 & prv == 78
	replace mun = 1		if muni == "ALMERIA" & regn == 8 & prv == 78
	replace mun = 8		if muni == "NAVAL" & regn == 8 & prv == 78
	
	replace muni = "SINDANGAN" if DHSCLUST == 416 & DHSYEAR == 2003
	replace muni = "DAPITAN CITY" if DHSCLUST == 410 & DHSYEAR == 2003
	replace muni = "PRES. MANUEL A. ROXAS" if DHSCLUST == 579 & DHSYEAR == 2008
	replace muni = "SERGIO OSMENA SR." if DHSCLUST == 580 & DHSYEAR == 2008
	replace muni = "SINDANGAN" if DHSCLUST == 582 & DHSYEAR == 2008
	replace mun = 1		if muni == "DAPITAN CITY" & regn == 9 & prv == 72
	replace mun = 8		if muni == "MUTIA" & regn == 9 & prv == 72
	replace mun = 10	if muni == "POLANCO" & regn == 9 & prv == 72
	replace mun = 15	if muni == "SIAYAN" & regn == 9 & prv == 72
	replace mun = 18 	if muni == "SINDANGAN" & regn == 9 & prv == 72
	replace mun = 7		if muni == "MANUKAN" & regn == 9 & prv == 72
	replace mun = 23	if muni == "GUTALAC" & regn == 9 & prv == 72
	replace mun = 2		if muni == "DIPOLOG CITY" & regn == 9 & prv == 72
	replace mun = 6		if muni == "LILOY" & regn == 9 & prv == 72
	replace mun = 16	if muni == "SIBUCO" & regn == 9 & prv == 72
	replace mun = 20	if muni == "SIRAWAI" & regn == 9 & prv == 72
	replace mun = 11 	if muni == "PRES. MANUEL A. ROXAS" & regn == 9 & prv == 72
	replace mun = 14 	if muni == "SERGIO OSMENA SR." & regn == 9 & prv == 72
	
	replace muni = "KUMALARANG" if DHSYEAR ==2003 & DHSCLUST == 421
	replace muni = "PAGADIAN CITY" if DHSCLUST == 587 & DHSYEAR == 2008
	replace muni = "JOSEFINA" if DHSCLUST == 589 & DHSYEAR == 2008
	replace muni = "TUKURAN" if DHSCLUST == 593 & DHSYEAR == 2008
	replace mun = 2		if muni == "AURORA" & regn == 9 & prv == 73
	replace mun = 17	if muni == "MARGOSATUBIG" & regn == 9 & prv == 73
	replace mun = 28	if muni == "TAMBULIG" & regn == 9 & prv == 73
	replace mun = 19	if muni == "MOLAVE" & regn == 9 & prv == 73
	replace mun = 22	if muni == "PAGADIAN CITY" & regn == 9 & prv == 73
	replace mun = 41	if muni == "VINCENZO A. SAGUN" & regn == 9 & prv == 73
	replace mun = 43	if muni == "GUIPOS" & regn == 9 & prv == 73
	replace mun = 32	if muni == "ZAMBOANGA" & regn == 9 & prv == 73
	replace mun = 8		if muni == "DUMINGAG" & regn == 9 & prv == 73
	replace mun = 18	if muni == "MIDSALIP" & regn == 9 & prv == 73
	replace mun = 27	if muni == "TABINA" & regn == 9 & prv == 73
	replace mun = 30	if muni == "TUKURAN" & regn == 9 & prv == 73
	replace mun = 37	if muni == "JOSEFINA" & regn == 9 & prv == 73 
	replace mun = 11	if muni == "KUMALARANG" & regn == 9 & prv == 73 
	
	replace muni = "BUUG" if DHSYEAR == 2008 & (DHSCLUST == 610 | DHSCLUST == 608)
	replace mun = 3		if muni == "DIPLAHAN" & regn == 9 & prv == 83
	replace mun = 8		if muni == "MALANGAS" & regn == 9 & prv == 83
	replace mun = 11	if muni == "PAYAO" & regn == 9 & prv == 83
	replace mun = 12	if muni == "ROSELLER T LIM" & regn == 9 & prv == 83
	replace mun = 13	if muni == "SIAY" & regn == 9 & prv == 83
	replace mun = 1		if muni == "ALICIA" & regn == 9 & prv == 83
	replace mun = 2		if muni == "BUUG" & regn == 9 & prv == 83
	
	replace mun = 8		if muni == "KIBAWE" & regn == 10 & prv == 13
	replace mun = 12	if muni == "MALAYBALAY CITY" & regn == 10 & prv == 13
	replace mun = 13	if muni == "MALITBOG" & regn == 10 & prv == 13
	replace mun = 14	if muni == "MANOLO FORTICH" & regn == 10 & prv == 13
	replace mun = 11	if muni == "LIBONA" & regn == 10 & prv == 13
	replace mun = 16	if muni == "PANGANTUCAN" & regn == 10 & prv == 13
	replace mun = 17	if muni == "QUEZON" & regn == 10 & prv == 13
	replace mun = 20	if muni == "TALAKAG" & regn == 10 & prv == 13
	replace mun = 21	if muni == "VALENCIA CITY" & regn == 10 & prv == 13
	replace mun = 22	if muni == "CABANGLASAN" & regn == 10 & prv == 13
	replace mun = 4		if muni == "DON CARLOS" & regn == 10 & prv == 13
	replace mun = 10	if muni == "LANTAPAN" & regn == 10 & prv == 13
	replace mun = 15	if muni == "MARAMAG" & regn == 10 & prv == 13
	
	replace mun = 3		if muni == "MAHINOG" & regn == 10 & prv == 18
	replace mun = 4		if muni == "MAMBAJAO" & regn == 10 & prv == 18
	
	replace muni = "BACOLOD" if DHSCLUST == 624 & DHSYEAR == 2008
	replace muni = "BALOI" if DHSCLUST == 625 & DHSYEAR == 2008
	replace muni = "ILIGAN CITY" if (DHSCLUST == 628 | DHSCLUST == 630) & DHSYEAR == 2008
	replace mun = 4		if muni == "ILIGAN CITY" & regn == 10 & prv == 35
	replace mun = 6		if muni == "SULTAN NAGA DIMAPORO" & regn == 10 & prv == 35
	replace mun = 9		if muni == "LALA" & regn == 10 & prv == 35
	replace mun = 3		if muni == "BAROY" & regn == 10 & prv == 35
	replace mun = 19	if muni == "SAPAD" & regn == 10 & prv == 35
	replace mun = 20	if muni == "TAGOLOAN" & regn == 10 & prv == 35
	replace mun = 1 	if muni == "BACOLOD" & regn == 10 & prv == 35
	replace mun = 2 	if muni == "BALOI" & regn == 10 & prv == 35
	replace mun = 23	if muni == "PANTAR" & regn == 10 & prv ==35
	
	replace muni = "OROQUIETA CITY" if DHSYEAR == 2003 & (DHSCLUST == 467 | DHSCLUST == 468)
	replace muni = "JIMENEZ" if DHSYEAR == 2008 & DHSCLUST == 632
	replace mun = 3		if muni == "BONIFACIO" & regn == 10 & prv == 42
	replace mun = 9		if muni == "OROQUIETA CITY" & regn == 10 & prv == 42
	replace mun = 1		if muni == "ALORAN" & regn == 10 & prv == 42
	replace mun = 10	if muni == "OZAMIZ CITY" & regn == 10 & prv == 42
	replace mun = 12	if muni == "PLARIDEL" & regn == 10 & prv == 42
	replace mun = 14	if muni == "SINACABAN" & regn == 10 & prv == 42
	replace mun = 7 	if muni == "JIMENEZ"
	
	replace muni = "SALAY" if DHSCLUST == 650 & DHSYEAR == 2008
	replace mun = 14	if muni == "LAGUINDINGAN" & regn == 10 & prv == 43
	replace mun = 2		if muni == "BALINGASAG" & regn == 10 & prv == 43
	replace mun = 6		if muni == "CLAVERIA" & regn == 10 & prv == 43
	replace mun = 8		if muni == "GINGOOG" & regn == 10 & prv == 43
	replace mun = 9		if muni == "GITAGUM" & regn == 10 & prv == 43
	replace mun = 5		if muni == "CAGAYAN DE ORO" & regn == 10 & prv == 43
	replace mun = 2		if muni == "BALINGASAG" & regn == 10 & prv == 43
	replace mun = 5		if muni == "CAGAYAN DE ORO" & regn == 10 & prv == 43
	replace mun = 6 	if muni == "CLAVERIA" & regn == 10 & prv == 43
	replace mun = 7		if muni == "EL SALVADOR CITY" & regn == 10 & prv == 43
	replace mun = 10	if muni == "INITAO" & regn == 10 & prv == 43
	replace mun = 11	if muni == "JASAAN" & regn == 10 & prv == 43
	replace mun = 18	if muni == "MANTICAO" & regn == 10 & prv == 43
	replace mun = 21	if muni == "OPOL" & regn == 10 & prv == 43
	replace mun = 22 	if muni == "SALAY" & regn == 10 & prv == 43
	replace mun = 12 	if muni == "KINOGUITAN" & regn == 10 & prv == 43
	
	replace muni = "ASUNCION" if DHSCLUST == 653 & DHSYEAR == 2008
	replace muni = "TAGUM CITY" if DHSCLUST == 656 & DHSYEAR == 2008
	replace prov = "DAVAO DEL SUR" if DHSCLUST == 661 & DHSYEAR == 2008
	replace mun = 3		if muni == "CARMEN" & regn == 11 & prv == 23
	replace mun = 14	if muni == "NEW CORELLA" & regn == 11 & prv == 23
	replace mun = 17	if muni == "ISLAND GARDEN CITY OF SAMAL" & regn == 11 & prv == 23
	replace mun = 19	if muni == "TAGUM CITY" & regn == 11 & prv == 23
	replace mun = 24	if muni == "SAN ISIDRO" & regn == 11 & prv == 23
	replace mun = 1		if muni == "ASUNCION" & regn == 11 & prv == 23
	replace mun = 15	if muni == "PANABO" & regn == 11 & prv == 23
	
	replace muni = "MARAGUSAN" if DHSCLUST == 686 & DHSYEAR == 2008
	replace muni = "LAAK" if muni == "LORETO" & regn == 11 & prv == 82
	replace mun = 9		if muni == "NABUNTURAAN" & regn == 11 & prv == 82
	replace mun = 7		if muni == "MONKAYO" & regn == 11 & prv == 82
	replace mun = 8		if muni == "MONTEVISTA" & regn == 11 & prv == 82
	replace mun = 9		if muni == "NABUNTURAN" & regn == 11 & prv == 82
	replace mun = 10	if muni == "NEW BATAAN" & regn == 11 & prv == 82
	replace mun = 11	if muni == "PANTUKAN" & regn == 11 & prv == 82
	replace mun = 2		if muni == "LAAK" & regn == 11 & prv == 82
	replace mun = 5		if muni == "MARAGUSAN" & regn == 11 & prv == 82

	replace muni = "DAVAO CITY" if DHSCLUST == 661 & DHSYEAR ==2008
	replace muni = "DAVAO CITY" if muni == "BUNAWAN" & regn == 11 & prv == 24
	replace mun = 2		if muni == "DAVAO CITY" & regn == 11 & prv == 24
	replace mun = 3		if muni == "DIGOS CITY" & regn == 11 & prv == 24
	replace mun = 5		if muni == "JOSE ABAD SANTOS" & regn == 11 & prv == 24
	replace mun = 6		if muni == "KIBLAWAN" & regn == 11 & prv == 24
	replace mun = 10	if muni == "MATANAO" & regn == 11 & prv == 24
	replace mun = 9		if muni == "MALITA" & regn == 11 & prv == 24
	replace mun = 7		if muni == "MAGSAYSAY" & regn == 11 & prv == 24
	replace mun = 16	if muni == "DON MARCELINO" & regn == 11 & prv == 24
	replace mun = 13	if muni == "SANTA MARIA" & regn == 11 & prv == 24
		
	replace mun = 1		if muni == "BAGANGA" & regn == 11 & prv == 25
	replace mun = 5		if muni == "CATEEL" & regn == 11 & prv == 25
	replace mun = 2		if muni == "BANAYBANAY" & regn == 11 & prv == 25
	replace mun = 9		if muni == "MATI" & regn == 11 & prv == 25
	replace mun = 10	if muni == "SAN ISIDRO" & regn == 11 & prv == 25
	
	replace muni = "KABAKAN" if DHSCLUST == 693 & DHSYEAR == 2008
	replace muni = "MIDSAYAP" if DHSCLUST == 700 & DHSYEAR == 2008
	replace muni = "PIGCAWAYAN" if (DHSCLUST == 774 | DHSCLUST == 775) & DHSYEAR == 2003
	replace mun = 1		if muni == "ALAMADA" & regn == 12 & prv == 47
	replace mun = 3		if muni == "KABACAN" & regn == 12 & prv == 47
	replace mun = 3		if muni == "KABAKAN" & regn == 12 & prv == 47
	replace mun = 8		if muni == "MATALAM" & regn == 12 & prv == 47
	replace mun = 9		if muni == "MIDSAYAP" & regn == 12 & prv == 47
	replace mun = 17	if muni == "ALEOSAN" & regn == 12 & prv == 47
	replace mun = 14	if muni == "TULUNAN" & regn == 12 & prv == 47
	replace mun = 16	if muni == "BANISILAN" & regn == 12 & prv == 47
	replace mun = 10	if muni == "M'LANG" & regn == 12 & prv == 47
	replace mun = 12	if muni == "PIKIT" & regn == 12 & prv == 47
	replace mun = 7		if muni == "MAKILALA" & regn == 12 & prv == 47
	replace mun = 2		if muni == "CARMEN" & regn == 12 & prv == 47
	replace mun = 13	if muni == "PRESIDENT ROXAS" & regn == 12 & prv == 47
	replace mun = 4		if muni == "KIDAPAWAN CITY" & regn == 12 & prv == 47
	replace mun = 11	if muni == "PIGCAWAYAN" & regn == 12 & prv == 47
	
	replace mun = 2		if muni == "BANGA" & regn == 12 & prv == 63
	replace mun = 3		if muni == "GENERAL SANTOS CITY" & regn == 12 & prv == 63
	replace mun = 12	if muni == "POLOMOLOK" & regn == 12 & prv == 63
	replace mun = 6 	if muni == "KORONADAL CITY" & regn == 12 & prv == 63
	replace mun = 11	if muni == "NORALA" & regn == 12 & prv == 63
	replace mun = 13	if muni == "SURALLAH" & regn == 12 & prv == 63
	replace mun = 16	if muni == "T'BOLI" & regn == 12 & prv == 63
	replace mun = 14	if muni == "TAMPAKAN" & regn == 12 & prv == 63
	
	replace muni = "LEBAK" if DHSYEAR == 2003 & DHSCLUST == 557
	replace mun = 4		if muni == "ISULAN" & regn == 12 & prv == 65
	replace mun = 1		if muni == "BAGUMBAYAN" & regn == 12 & prv == 65
	replace mun = 9		if muni == "PALIMBANG" & regn == 12 & prv == 65
	replace mun = 10	if muni == "PRESIDENT QUIRINO" & regn == 12 & prv == 65
	replace mun = 6		if muni == "LEBAK" & regn == 12 & prv == 65
	replace mun = 12	if muni == "SENATOR NINOY AQUINO" & regn == 12 & prv == 65
		
	replace mun = 1		if muni == "ALABEL" & regn == 12 & prv == 80
	replace mun = 2		if muni == "GLAN" & regn == 12 & prv == 80
	replace mun = 3		if muni == "KIAMBA" & regn == 12 & prv == 80
	replace mun = 7		if muni == "MALUNGON" & regn == 12 & prv == 80
	replace mun = 6		if muni == "MALAPATAN" & regn == 12 & prv == 80
	replace mun = 4		if muni == "MAASIM" & regn == 12 & prv == 80
	
	replace mun = 99	if muni == "MANILA" & regn == 13 & prv == 39

	replace mun = 1		if muni == "MANDALUYONG" & regn == 13 & prv == 74
	replace mun = 2		if muni == "MARIKINA" & regn == 13 & prv == 74
	replace mun = 3		if muni == "PASIG" & regn == 13 & prv == 74
	replace mun = 4		if muni == "QUEZON CITY" & regn == 13 & prv == 74
	replace mun = 4		if muni == "LUNGSOD QUEZON" & regn == 13 & prv == 74
	replace mun = 5		if muni == "SAN JUAN" & regn == 13 & prv == 74

	replace mun = 1		if muni == "CALOOCAN" & regn == 13 & prv == 75
	replace mun = 2		if muni == "MALABON" & regn == 13 & prv == 75
	replace mun = 2		if muni == "CITY OF MALABON" & regn == 13 & prv == 75
	replace mun = 3		if muni == "NAVOTAS" & regn == 13 & prv == 75
	replace mun = 3		if muni == "CITY OF NAVOTAS" & regn == 13 & prv == 75
	replace mun = 4		if muni == "VALENZUELA" & regn == 13 & prv == 75
	replace mun = 4		if muni == "LUNGSOD NG VALENZUELA" & regn == 13 & prv == 75
	
	replace mun = 1		if muni == "LAS PINAS" & regn == 13 & prv == 76
	replace mun = 2		if muni == "MAKATI" & regn == 13 & prv == 76
	replace mun = 3		if muni == "MUTINLUPA" & regn == 13 & prv == 76
	replace mun = 3		if muni == "MUNTINLUPA" & regn == 13 & prv == 76
	replace mun = 4		if muni == "PARANAQUE" & regn == 13 & prv == 76
	replace mun = 4		if muni == "PARAñAQUE" & regn == 13 & prv == 76
	replace mun = 5		if muni == "PASAY" & regn == 13 & prv == 76
	replace mun = 6		if muni == "PATEROS" & regn == 13 & prv == 76
	replace mun = 7		if muni == "TAGUIG" & regn == 13 & prv == 76
	
	replace mun = 1		if muni == "BANGUED" & regn == 14 & prv == 1
	replace mun = 8		if muni == "LA PAZ" & regn == 14 & prv == 1
	replace mun = 13	if muni == "LICUAN-BAAY" & regn == 14 & prv == 1
	replace mun = 14	if muni == "LUBA" & regn == 14 & prv == 1
		
	replace mun = 2		if muni == "BANGUIO" & regn == 14 & prv == 11
	replace mun = 6 	if muni == "ITOGON" & regn == 14 & prv == 11
	replace mun = 10	if muni == "LA TRINIDAD" & regn == 14 & prv == 11		
	replace mun = 12	if muni == "SABLAN" & regn == 14 & prv == 11
	replace mun = 13	if muni == "TUBA" & regn == 14 & prv == 11
	replace mun = 2		if muni == "BAGUIO" & regn == 14 & prv == 11
	replace mun = 1		if muni == "ATOK" & regn == 14 & prv == 11
	replace mun = 11 	if muni == "MANKAYAN" & regn == 14 & prv == 11
	replace mun = 7		if muni == "KABAYAN" & regn == 14 & prv == 11
	replace mun = 8		if muni == "KAPANGAN" & regn == 14 & prv == 11
	
	replace muni = "LAGAWE" if DHSYEAR == 2003 & DHSCLUST == 743
	replace mun = 6		if muni == "MAYOYAO" & regn == 14 & prv == 27
	replace mun = 8		if muni == "AGUINALDO" & regn == 14 & prv == 27
	replace mun = 7		if muni == "ALFONSO LISTA" & regn == 14 & prv == 27
	replace mun = 10	if muni == "TINOC" & regn == 14 & prv == 27
	replace mun = 4		if muni == "LAGAWE" & regn == 14 & prv == 27
	
	replace mun = 13	if muni == "TABUK" & regn == 14 & prv == 32
	replace mun = 9		if muni == "PINUKPUK" & regn == 14 & prv == 32
	replace mun = 15	if muni == "TINGLAYAN" & regn == 14 & prv == 32
	
	replace mun = 8		if muni == "SADANGA" & regn == 14 & prv == 44
	replace mun = 5		if muni == "NATONIN" & regn ==14 & prv == 44
	replace mun = 7		if muni == "SABANGAN" & regn == 14 & prv == 44
	replace mun = 4		if muni == "BONTOC" & regn == 14 & prv == 44
	replace mun = 2		if muni == "BAUKO" & regn == 14 & prv == 44
	replace mun = 10	if muni == "TADIAN" & regn == 14 & prv == 44
	
	replace mun = 2		if muni == "CONNER" & regn == 14 & prv == 81
	replace mun = 5		if muni == "LUNA" & regn == 14 & prv == 81
	replace mun = 1		if muni == "CALANASAN" & regn == 14 & prv == 81
	
	replace muni = "BALINDONG" if DHSCLUST == 733 & DHSYEAR == 2008
	replace mun = 2		if muni == "LAMITAN CITY" & regn == 15 & prv == 7
	replace mun = 7		if muni == "TUBURAN" & regn == 15 & prv == 7
	replace mun = 5		if muni == "SUMISIP" & regn == 15 & prv == 7
	
	replace muni = "WAO" if DHSYEAR == 2003 & DHSCLUST == 761
	replace muni = "SULTAN DUMALONDONG" if DHSYEAR == 2008 & DHSCLUST == 740
	replace muni = "SAGUIARAN" if muni == "LA TRINIDAD" & regn == 15 & prv == 36
	replace mun = 40	if muni == "SULTAN DUMALONDONG" & regn == 15 & prv == 36
	replace mun = 29	if muni == "TUGAYA" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 34	if muni == "MAGUING" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 36	if muni == "LUMBAYANAGUE" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 17	if muni == "MARAWI CITY" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 10	if muni == "KAPAI" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 7		if muni == "BUTIG" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 30	if muni == "WAO" & (regn == 15 | regn ==10) & prv == 36
	replace mun = 3		if muni == "BALINDONG" & regn == 15 & prv == 36
	replace mun = 20	if muni == "PAGAYAWAN" & regn == 15 & prv == 36
	replace mun = 28	if muni == "TUBARAN" & regn == 15 & prv == 36
	replace mun = 25	if muni == "SAGUIARAN" & regn == 15 & prv == 36
	replace mun = 11	if muni == "LUMBA - BAYABAO" & regn == 15 & prv == 36
	
	replace muni = "DATU ANGGAL MIDTIMBANG" if DHSCLUST == 749 & DHSYEAR == 2008
	replace muni = "SULTAN KUDARAT" if DHSCLUST == 769 & DHSYEAR == 2003
	replace mun = 1		if muni == "AMPATUAN" & regn == 15  & prv == 38
	replace mun = 2		if muni == "BULDON" & regn == 15  & prv == 38
	replace mun = 6		if muni == "DATU PIANG" & regn == 15  & prv == 38
	replace mun = 15	if muni == "UPI" & regn == 15  & prv == 38
	replace mun = 20	if muni == "MAMASAPANO" & regn == 15  & prv == 38
	replace mun = 12	if muni == "SULTAN KUDARAT" & regn == 15  & prv == 38
	replace mun = 13	if muni == "SULTAN SA BARONGIS" & regn == 15  & prv == 38
	replace mun = 30	if muni == "DATU BLAH SINSUAT" & regn == 15  & prv == 38
	replace mun = 4		if muni == "COTABATO CITY" & regn == 15  & prv == 38
	replace mun = 17	if muni == "SOUTH UPI" & regn == 15 & prv == 38
	replace mun = 24	if muni == "SULTAN MASTURA" & regn == 15 & prv == 38
	replace mun = 21	if muni == "TALITAY" & regn == 15 & prv == 38
	replace mun = 11	if muni == "PARANG" & regn == 15 & prv == 38
	replace mun = 9		if muni == "MATANOG" & regn == 15 & prv == 38
	replace mun = 22	if muni == "PAGAGAWAN" & regn == 15 & prv == 38
	replace mun = 3		if muni == "BULUAN" & regn == 15 & prv == 38
	replace mun = 31	if muni == "DATU ANGGAL MIDTIMBANG" & regn == 15 & prv == 38
	
	replace muni = "TONGKIL" if DHSYEAR == 2003 & DHSCLUST == 783
	replace mun = 2		if muni == "JOLO" & regn == 15 & prv == 66
	replace mun = 11	if muni == "PATIKUL" & regn == 15 & prv == 66
	replace mun = 12	if muni == "SAISI" & regn == 15 & prv == 66
	replace mun = 13	if muni == "TALIPAO" & regn == 15 & prv == 66
	replace mun = 6		if muni == "HADJI PANGLIMA TAHIL" & regn == 15 & prv == 66
	replace mun = 7		if muni == "OLD PANAMAO" & regn == 15 & prv == 66
	replace mun = 12 	if muni == "SIASI" & regn == 15 & prv == 66
	replace mun = 3		if muni == "KALINGALAN CALUANG" & regn == 15 & prv == 66
	replace mun = 4 	if muni == "LUUK" & regn == 15 & prv == 66
	replace mun = 15	if muni == "TONGKIL" & regn == 15 & prv == 66
	
	replace mun = 2		if muni == "BONGAO" & regn == 15 & prv == 70
	replace mun = 4		if muni == "SIMUNUL" & regn == 15 & prv == 70
	replace mun = 9		if muni == "LANGUYAN" & regn == 15 & prv == 70
	replace mun = 11	if muni == "SIBUTU" & regn == 15 & prv == 70
	replace mun = 10 	if muni == "SAPA-SAPA" & regn == 15 & prv == 70
	
	replace muni = "LAS NIEVES" if DHSYEAR == 2003 & DHSCLUST == 792
	replace mun = 2		if muni == "BUTUAN CITY" & regn == 16 & prv == 2 
	replace mun = 8		if muni == "MAGALLANES" & regn == 16 & prv == 2 
	replace mun = 10	if muni == "SANTIAGO" & regn == 16 & prv == 2 
	replace mun = 12	if muni == "REMEDIOS T. ROMUALDEZ" & regn == 16 & prv == 2 
	replace mun = 3		if muni == "CABADBARAN CITY" & regn == 16 & prv == 2
	replace mun = 5		if muni == "JABONGA" & regn == 16 & prv == 2
	replace mun = 4	 	if muni == "CARMEN" & regn == 16 & prv == 2
	replace mun = 7	 	if muni == "LAS NIEVES" & regn == 16 & prv == 2
		
	replace mun = 1		if muni == "BAYUGAN CITY" & regn == 16 & prv == 3
	replace mun = 4		if muni == "LA PAZ" & regn == 16 & prv == 3
	replace mun = 6		if muni == "PROSPERIDAD" & regn == 16 & prv == 3
	replace mun = 7		if muni == "ROSARIO" & regn == 16 & prv == 3
	replace mun = 8		if muni == "SAN FRANCISCO" & regn == 16 & prv == 3
	replace mun = 9		if muni == "SAN LUIS" & regn == 16 & prv == 3
	replace mun = 3		if muni == "ESPERANZA" & regn == 16 & prv == 3
	replace mun = 10	if muni == "SANTA JOSEFA" & regn == 16 & prv == 3
	
	replace muni = "SISON" if DHSYEAR == 2008 & DHSCLUST == 783
	replace mun = 7		if muni == "DAPA" & regn == 16 & prv == 67
	replace mun = 8		if muni == "DEL CARMEN" & regn == 16 & prv == 67
	replace mun = 10	if muni == "GENERAL LUNA" & regn == 16 & prv == 67
	replace mun = 24	if muni == "SURIGAO CITY" & regn == 16 & prv == 67
	replace mun = 17	if muni == "PLACER" & regn == 16 & prv == 67
	replace mun = 27	if muni == "TUBOD" & regn == 16 & prv == 67
	replace mun = 14	if muni == "MAINIT" & regn == 16 & prv == 67
	replace mun = 22 	if muni == "SISON" & regn == 16 & prv == 67
	
	replace muni = "BAYABAS" if DHSYEAR == 2008 & DHSCLUST == 787
	replace muni = "HINATUAN" if DHSYEAR == 2008 & DHSCLUST == 793
	replace mun = 3		if muni == "BISLIG" & regn == 16 & prv == 68
	replace mun = 4		if muni == "CAGWAIT" & regn == 16 & prv == 68
	replace mun = 5		if muni == "CANTILAN" & regn == 16 & prv == 68
	replace mun = 13	if muni == "MADRID" & regn == 16 & prv == 68
	replace mun = 10	if muni == "LANUZA" & regn == 16 & prv == 68
	replace mun = 16	if muni == "SAN MIGUEL" & regn == 16 & prv == 68
	replace mun = 18	if muni == "TAGO" & regn == 16 & prv == 68
	replace mun = 7		if muni == "CARRASCAL" & regn == 16 & prv == 68
	replace mun = 19	if muni == "TANDAG" & regn == 16 & prv == 68
	replace mun = 9 	if muni == "HINATUAN" & regn == 16 & prv == 68
	replace mun = 2 	if muni == "BAYABAS" & regn == 16 & prv == 68
	
	replace muni = "CAGDIANAO" if DHSYEAR == 2008 & DHSCLUST == 780
	replace mun = 4		if muni == "LIBJO" & regn == 16 & prv == 85
	replace mun = 2		if muni == "CAGDIANAO" & regn == 16 & prv == 85
	replace mun = 1		if muni == "BASILISA" & regn == 16 * prv == 85
	
	replace mun = 3		if muni == "GASAN" & regn == 17 & prv == 40
	replace mun = 5		if muni == "SANTA CRUZ" & regn == 17 & prv == 40
	replace mun = 4		if muni == "MOGPOG" & regn == 17 & prv == 40
	
	replace mun = 1		if muni == "ABRA DE IILOG" & regn == 17 & prv == 51
	replace mun = 5		if muni == "MAGSAYSAY" & regn == 17 & prv == 51
	replace mun = 9		if muni == "SABLAYAN" & regn == 17 & prv == 51	
	replace mun = 1		if muni == "ABRA DE ILOG" & regn == 17 & prv == 51
	replace mun = 7		if muni == "PALUAN" & regn == 17 & prv == 51
	replace mun = 6		if muni == "MAMBURAO" & regn == 17 & prv == 51
	replace mun = 10	if muni == "SAN JOSE" & regn == 17 & prv == 51
	replace mun = 11	if muni == "SANTA CRUZ" & regn == 17 & prv == 51
		
	replace mun = 1		if muni == "BACO" & regn == 17 & prv == 52
	replace mun = 2		if muni == "BANSUD" & regn == 17 & prv == 52
	replace mun = 3		if muni == "BONGABONG" & regn == 17 & prv == 52
	replace mun = 12	if muni == "ROXAS" & regn == 17 & prv == 52
	replace mun = 10	if muni == "POLA" & regn == 17 & prv == 52
	replace mun = 15	if muni == "VICTORIA" & regn == 17 & prv == 52
	replace mun = 9		if muni == "PINAMALAYAN" & regn == 17 & prv == 52
	replace mun = 4		if muni == "BULALACAO" & regn == 17 & prv == 52
	replace mun = 5		if muni == "CALAPAN" & regn == 17 & prv == 52
	replace mun = 6		if muni == "GLORIA" & regn == 17 & prv == 52
	replace mun = 11	if muni == "PUERTO GALERA" & regn == 17 & prv == 52	
	
	replace mun = 1		if muni == "ABORLAN" & regn == 17 & prv == 53
	replace mun = 10	if muni == "CUYO" & regn == 17 & prv == 53
	replace mun = 16	if muni == "PUERTO PRINCESA" & regn == 17 & prv == 53
	replace mun = 17	if muni == "QUEZON" & regn == 17 & prv == 53
	replace mun = 18	if muni == "ROZXAS" & regn == 17 & prv == 53
	replace mun = 24	if muni == "SOFRONIO ESPANOLA" & regn == 17 & prv == 53
	replace mun = 15	if muni == "NARRA" & regn == 17 & prv == 53
	replace mun = 5		if muni == "BATARAZA" & regn == 17 & prv == 53
	replace mun = 24	if muni == "SOFRONIO ESPAñOLA" & regn == 17 & prv == 53
	replace mun = 20	if muni == "TAYTAY" & regn == 17 & prv == 53
	replace mun = 6		if muni == "BROOKE'S POINT" & regn == 17 & prv == 53
	replace mun = 19	if muni == "SAN VICENTE" & regn == 17 & prv == 53
		
	replace mun = 4		if muni == "CALATRAVA" & regn == 17 & prv == 59
	replace mun = 6		if muni == "CORCUERA" & regn == 17 & prv == 59
	replace mun = 7		if muni == "LOOC" & regn == 17 & prv == 59
	replace mun = 11	if muni == "SAN AUGUSTIN" & regn == 17 & prv == 59
	replace mun = 13	if muni == "SAN FERNANDO" & regn == 17 & prv == 59
	replace mun = 11	if muni == "SAN AGUSTIN" & regn == 17 & prv == 59
	
	replace regn = 15 	if regn == 10 & prv == 36 & mun == 10 & prov == "LANAO DEL SUR"
	
	drop if muni == ""
	drop if mun == .
	
	drop X
	
	save "$geodata/2003_2008geocodes_matched.dta", replace
	

*** SET UP 1998 and 2013 Municipality codes

	use "$input2013/Household 2013 PHHR61FL.dta", clear
	
	gen regn=1 if hv024==3
	replace regn=2 if hv024==4
	replace regn=3 if hv024==5
	replace regn=4 if hv024==6
	replace regn=5 if hv024==8
	replace regn=6 if hv024==9
	replace regn=7 if hv024==10
	replace regn=8 if hv024==11
	replace regn=9 if hv024==12
	replace regn=10 if hv024==13
	replace regn=11 if hv024==14
	replace regn=12 if hv024==15
	replace regn=13 if hv024==1
	replace regn=14 if hv024==2
	replace regn=15 if hv024==17
	replace regn=16 if hv024==16
	replace regn=17 if hv024==7
		
	gen DHSYEAR = 2013
	
	keep regn shprov shmuni hv001 DHSYEAR
	
	rename shprov prv 
	rename shmuni mun
	rename hv001 DHSCLUST
	
	replace mun=99 if prv==39
	replace prv=38 if prv==98&mun==4
	replace regn=15 if prv==38&mun==4

	duplicates drop DHSCLUST, force
	
	save "$geodata/2013geocodes.dta", replace


	** 1998 geocodes for muni 
	use "$input1998/Household 1998 PHHR3BFL.dta", clear
	
	gen DHSYEAR = 1998
	rename hv024 regn
	rename shprov prv
	rename shctymun mun
	
	replace regn=17 if inlist(prv, 40, 53, 59, 52, 51)
	replace regn=3 if prv==77
	replace regn=15 if prv==7
	replace prv=83 if inlist(mun, 4, 9, 10, 16, 26, 31, 36)&prv==73
	replace mun=13 if mun==26&prv==83 								//Siay muni
	replace mun=16 if mun==31&prv==83 								//Tungawan muni
	replace mun=4 if mun==36&prv==83 								//Imelda muni
	replace mun=15 if prv==73&mun==42 								//Mahayag muni
	replace regn=12 if prv==63|prv==80
	replace prv=82 if inlist(mun, 4, 8, 9, 10, 16)&prv==23
	replace mun=11 if prv==82&mun==16								//Pantukan muni
	replace regn=10 if prv==35
	replace mun=99 if prv==39
	replace prv=85 if (mun==3|mun==5)&prv==67
	replace mun=16 if prv==62&mun==1
	replace prv=36 if prv==98&mun==17
	replace prv=38 if prv==98&mun==4
	replace regn=15 if prv==36&mun==17
	replace regn=15 if prv==38&mun==4

	keep regn prv mun hv001 DHSYEAR
	
	rename hv001 DHSCLUST
	
	duplicates drop DHSCLUST, force
	
	save "$geodata/1998geocodes.dta", replace

** COMBINE MUNI_PROV_REGN across DHS rounds
	
	use "$geodata/2003_2008geocodes_matched.dta", clear
		append using "$geodata/2013geocodes.dta"
		append using "$geodata/1998geocodes.dta"
	save "$geodata/1998_to_2013geocodes_matched.dta", replace
		 
