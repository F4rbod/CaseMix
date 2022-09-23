#install.packages("packrat")
#packrat::init("./")
packrat::on()

options(repr.matrix.max.rows=100, repr.matrix.max.cols=300)
options(repr.plot.width = 20, repr.plot.height = 15)
options(width=300)

numcores=56

#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("fst")
#install.packages("comorbidity")
#install.packages("reshape")
#install.packages("dtplyr")
#install.packages("haven")
#install.packages("vroom")
#install.packages("dplyr")



library(tidyverse)
library(data.table)
library(fst)
library(comorbidity)
library(reshape)
library(dtplyr)
library(haven)
library(vroom)
library(dplyr)
library(stringr)
library(fastmatch)

`%!in%` = Negate(`%in%`)

setDTthreads(numcores)









#diagnosis codes

office_visit_codes <- c(
  "99201", "99202", "99203", "99204", "99205", "99211", "99212", "99213", "99214",
  "99215"
)


non_us_state_codes <- c(40, 54, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 97, 98, 99)
primary_care_specialty_codes <- c("01", "08", "11", "38")


race_codes <- data.frame(
  race_code = seq(0, 6),
  race = c("Unknown", "White", "Black", "Other", "Asian", "Hispanic", "North American Native")
)

sex_codes <- data.frame(
  sex_code = seq(0, 2),
  sex = c("Unknown", "Male", "Female")
)


hypertension_icd_9_codes <- c("36211","4010","4011","4019","40200","40201","40210","40211","40290","40291","40300","40301","40310","40311","40390","40391","40400","40401","40402","40403","40410","40411","40412","40413","40490","40491","40492","40493","40501","40509","40511","40519","40591","40599","4372")
hypertension_icd_10_codes <-c("H35031","H35032","H35033","H35039","I10","I110","I119","I120","I129","I130","I1310","I1311","I132","I150","I151","I152","I158","I159","I674","N262")


IHD_icd_9_codes <- c("41000","41001","41002","41010","41011","41012","41020","41021","41022","41030","41031","41032","41040","41041","41042","41050","41051","41052","41060","41061","41062","41070","41071","41072","41080","41081","41082","41090","41091","41092","4110","4111","41181","41189","412","4130","4131","4139","41400","41401","41402","41403","41404","41405","41406","41407","41412","4142","4143","4144","4148","4149")
IHD_icd_10_codes <- c("I200","I201","I208","I209","I2101","I2102","I2109","I2111","I2119","I2121","I2129","I213","I214","I21A1","I21A9","I220","I221","I222","I228","I229","I230","I231","I232","I233","I234","I235","I236","I237","I238","I240","I241","I248","I249","I2510","I25110","I25111","I25118","I25119","I252","I253","I2541","I2542","I255","I256","I25700","I25701","I25708","I25709","I25710","I25711","I25718","I25719","I25720","I25721","I25728","I25729","I25730","I25731","I25738","I25739","I25750","I25751","I25758","I25759","I25760","I25761","I25768","I25769","I25790","I25791","I25798","I25799","I25810","I25811","I25812","I2582","I2583","I2584","I2589","I259")


depression_icd_9_codes <- c("29620","29621","29622","29623","29624","29625","29626","29630","29631","29632","29633","29634","29635","29636","29651","29652","29653","29654","29655","29656","29660","29661","29662","29663","29664","29665","29666","29689","2980","3004","3091","311")
depression_icd_10_codes <- c("F3130","F3131","F3132","F314","F315","F3160","F3161","F3162","F3163","F3164","F3175","F3176","F3177","F3178","F3181","F320","F321","F322","F323","F324","F325","F329","F330","F331","F332","F333","F3340","F3341","F3342","F338","F339","F341","F4321","F4323")



diabetes_icd_9_codes <- c("24900","24901","24910","24911","24920","24921","24930","24931","24940","24941","24950","24951","24960","24961","24970","24971","24980","24981","24990","24991","25000","25001","25002","25003","25010","25011","25012","25013","25020","25021","25022","25023","25030","25031","25032","25033","25040","25041","25042","25043","25050","25051","25052","25053","25060","25061","25062","25063","25070","25071","25072","25073","25080","25081","25082","25083","25090","25091","25092","25093","3572","36201","36202","36203","36204","36205","36206","36641")
diabetes_icd_10_codes <- c("E0800","E0801","E0810","E0811","E0821","E0822","E0829","E08311","E08319","E08321","E083211","E083212","E083213","E083219","E08329","E083291","E083292","E083293","E083299","E08331","E083311","E083312","E083313","E083319","E08339","E083391","E083392","E083393","E083399","E08341","E083411","E083412","E083413","E083419","E08349","E083491","E083492","E083493","E083499","E08351","E083511","E083512","E083513","E083519","E083521","E083522","E083523","E083529","E083531","E083532","E083533","E083539","E083541","E083542","E083543","E083549","E083551","E083552","E083553","E083559","E08359","E083591","E083592","E083593","E083599","E0836","E0837X1","E0837X2","E0837X3","E0837X9","E0839","E0840","E0841","E0842","E0843","E0844","E0849","E0851","E0852","E0859","E08610","E08618","E08620","E08621","E08622","E08628","E08630","E08638","E08641","E08649","E0865","E0869","E088","E089","E0900","E0901","E0910","E0911","E0921","E0922","E0929","E09311","E09319","E09321","E093211","E093212","E093213","E093219","E09329","E093291","E093292","E093293","E093299","E09331","E093311","E093312","E093313","E093319","E09339","E093391","E093392","E093393","E093399","E09341","E093411","E093412","E093413","E093419","E09349","E093491","E093492","E093493","E093499","E09351","E093511","E093512","E093513","E093519","E093521","E093522","E093523","E093529","E093531","E093532","E093533","E093539","E093541","E093542","E093543","E093549","E093551","E093552","E093553","E093559","E09359","E093591","E093592","E093593","E093599","E0936","E0937X1","E0937X2","E0937X3","E0937X9","E0939","E0940","E0941","E0942","E0943","E0944","E0949","E0951","E0952","E0959","E09610","E09618","E09620","E09621","E09622","E09628","E09630","E09638","E09641","E09649","E0965","E0969","E098","E099","E1010","E1011","E1021","E1022","E1029","E10311","E10319","E10321","E103211","E103212","E103213","E103219","E10329","E103291","E103292","E103293","E103299","E10331","E103311","E103312","E103313","E103319","E10339","E103391","E103392","E103393","E103399","E10341","E103411","E103412","E103413","E103419","E10349","E103491","E103492","E103493","E103499","E10351","E103511","E103512","E103513","E103519","E103521","E103522","E103523","E103529","E103531","E103532","E103533","E103539","E103541","E103542","E103543","E103549","E103551","E103552","E103553","E103559","E10359","E103591","E103592","E103593","E103599","E1036","E1037X1","E1037X2","E1037X3","E1037X9","E1039","E1040","E1041","E1042","E1043","E1044","E1049","E1051","E1052","E1059","E10610","E10618","E10620","E10621","E10622","E10628","E10630","E10638","E10641","E10649","E1065","E1069","E108","E109","E1100","E1101","E1110","E1111","E1121","E1122","E1129","E11311","E11319","E11321","E113211","E113212","E113213","E113219","E11329","E113291","E113292","E113293","E113299","E11331","E113311","E113312","E113313","E113319","E11339","E113391","E113392","E113393","E113399","E11341","E113411","E113412","E113413","E113419","E11349","E113491","E113492","E113493","E113499","E11351","E113511","E113512","E113513","E113519","E113521","E113522","E113523","E113529","E113531","E113532","E113533","E113539","E113541","E113542","E113543","E113549","E113551","E113552","E113553","E113559","E11359","E113591","E113592","E113593","E113599","E1136","E1137X1","E1137X2","E1137X3","E1137X9","E1139","E1140","E1141","E1142","E1143","E1144","E1149","E1151","E1152","E1159","E11610","E11618","E11620","E11621","E11622","E11628","E11630","E11638","E11641","E11649","E1165","E1169","E118","E119","E1300","E1301","E1310","E1311","E1321","E1322","E1329","E13311","E13319","E13321","E133211","E133212","E133213","E133219","E13329","E133291","E133292","E133293","E133299","E13331","E133311","E133312","E133313","E133319","E13339","E133391","E133392","E133393","E133399","E13341","E133411","E133412","E133413","E133419","E13349","E133491","E133492","E133493","E133499","E13351","E133511","E133512","E133513","E133519","E133521","E133522","E133523","E133529","E133531","E133532","E133533","E133539","E133541","E133542","E133543","E133549","E133551","E133552","E133553","E133559","E13359","E133591","E133592","E133593","E133599","E1336","E1339","E1340","E1341","E1342","E1343","E1344","E1349","E1351","E1352","E1359","E13610","E13618","E13620","E13621","E13622","E13628","E13630","E13638","E13641","E13649","E1365","E1369","E138","E139")



arthritis_icd_9_codes <- c("7140","7141","7142","71430","71431","71432","71433","71500","71504","71509","71510","71511","71512","71513","71514","71515","71516","71517","71518","71520","71521","71522","71523","71524","71525","71526","71527","71528","71530","71531","71532","71533","71534","71535","71536","71537","71538","71580","71589","71590","71591","71592","71593","71594","71595","71596","71597","71598","7200","7210","7211","7212","7213","72190","72191")
arthritis_icd_10_codes <- c("M0500","M05011","M05012","M05019","M05021","M05022","M05029","M05031","M05032","M05039","M05041","M05042","M05049","M05051","M05052","M05059","M05061","M05062","M05069","M05071","M05072","M05079","M0509","M0520","M05211","M05212","M05219","M05221","M05222","M05229","M05231","M05232","M05239","M05241","M05242","M05249","M05251","M05252","M05259","M05261","M05262","M05269","M05271","M05272","M05279","M0529","M0530","M05311","M05312","M05319","M05321","M05322","M05329","M05331","M05332","M05339","M05341","M05342","M05349","M05351","M05352","M05359","M05361","M05362","M05369","M05371","M05372","M05379","M0539","M0540","M05411","M05412","M05419","M05421","M05422","M05429","M05431","M05432","M05439","M05441","M05442","M05449","M05451","M05452","M05459","M05461","M05462","M05469","M05471","M05472","M05479","M0549","M0550","M05511","M05512","M05519","M05521","M05522","M05529","M05531","M05532","M05539","M05541","M05542","M05549","M05551","M05552","M05559","M05561","M05562","M05569","M05571","M05572","M05579","M0559","M0560","M05611","M05612","M05619","M05621","M05622","M05629","M05631","M05632","M05639","M05641","M05642","M05649","M05651","M05652","M05659","M05661","M05662","M05669","M05671","M05672","M05679","M0569","M0570","M05711","M05712","M05719","M05721","M05722","M05729","M05731","M05732","M05739","M05741","M05742","M05749","M05751","M05752","M05759","M05761","M05762","M05769","M05771","M05772","M05779","M0579","M057A","M0580","M05811","M05812","M05819","M05821","M05822","M05829","M05831","M05832","M05839","M05841","M05842","M05849","M05851","M05852","M05859","M05861","M05862","M05869","M05871","M05872","M05879","M0589","M058A","M059","M0600","M06011","M06012","M06019","M06021","M06022","M06029","M06031","M06032","M06039","M06041","M06042","M06049","M06051","M06052","M06059","M06061","M06062","M06069","M06071","M06072","M06079","M0608","M0609","M060A","M061","M0620","M06211","M06212","M06219","M06221","M06222","M06229","M06231","M06232","M06239","M06241","M06242","M06249","M06251","M06252","M06259","M06261","M06262","M06269","M06271","M06272","M06279","M0628","M0629","M0630","M06311","M06312","M06319","M06321","M06322","M06329","M06331","M06332","M06339","M06341","M06342","M06349","M06351","M06352","M06359","M06361","M06362","M06369","M06371","M06372","M06379","M0638","M0639","M0680","M06811","M06812","M06819","M06821","M06822","M06829","M06831","M06832","M06839","M06841","M06842","M06849","M06851","M06852","M06859","M06861","M06862","M06869","M06871","M06872","M06879","M0688","M0689","M068A","M069","M0800","M08011","M08012","M08019","M08021","M08022","M08029","M08031","M08032","M08039","M08041","M08042","M08049","M08051","M08052","M08059","M08061","M08062","M08069","M08071","M08072","M08079","M0808","M0809","M080A","M081","M0820","M08211","M08212","M08219","M08221","M08222","M08229","M08231","M08232","M08239","M08241","M08242","M08249","M08251","M08252","M08259","M08261","M08262","M08269","M08271","M08272","M08279","M0828","M0829","M082A","M083","M0840","M08411","M08412","M08419","M08421","M08422","M08429","M08431","M08432","M08439","M08441","M08442","M08449","M08451","M08452","M08459","M08461","M08462","M08469","M08471","M08472","M08479","M0848","M084A","M0880","M08811","M08812","M08819","M08821","M08822","M08829","M08831","M08832","M08839","M08841","M08842","M08849","M08851","M08852","M08859","M08861","M08862","M08869","M08871","M08872","M08879","M0888","M0889","M0890","M08911","M08912","M08919","M08921","M08922","M08929","M08931","M08932","M08939","M08941","M08942","M08949","M08951","M08952","M08959","M08961","M08962","M08969","M08971","M08972","M08979","M0898","M0899","M089A","M150","M151","M152","M153","M154","M158","M159","M160","M1610","M1611","M1612","M162","M1630","M1631","M1632","M164","M1650","M1651","M1652","M166","M167","M169","M170","M1710","M1711","M1712","M172","M1730","M1731","M1732","M174","M175","M179","M180","M1810","M1811","M1812","M182","M1830","M1831","M1832","M184","M1850","M1851","M1852","M189","M19011","M19012","M19019","M19021","M19022","M19029","M19031","M19032","M19039","M19041","M19042","M19049","M19071","M19072","M19079","M1909","M19111","M19112","M19119","M19121","M19122","M19129","M19131","M19132","M19139","M19141","M19142","M19149","M19171","M19172","M19179","M1919","M19211","M19212","M19219","M19221","M19222","M19229","M19231","M19232","M19239","M19241","M19242","M19249","M19271","M19272","M19279","M1929","M1990","M1991","M1992","M1993","M450","M451","M452","M453","M454","M455","M456","M457","M458","M459","M47011","M47012","M47013","M47014","M47015","M47016","M47019","M47021","M47022","M47029","M4710","M4711","M4712","M4713","M4720","M4721","M4722","M4723","M4724","M4725","M4726","M4727","M4728","M47811","M47812","M47813","M47814","M47815","M47816","M47817","M47818","M47819","M47891","M47892","M47893","M47894","M47895","M47896","M47897","M47898","M47899","M479","M488X1","M488X2","M488X3","M488X4","M488X5","M488X6","M488X7","M488X8","M488X9")








claim_carrier_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_carrier_all_years.fst",as.data.table = T)







claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), hypertension_icd_9_codes)), is_hypertension:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), hypertension_icd_10_codes)), is_hypertension:=T]


claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), arthritis_icd_9_codes)), is_arthritis:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), arthritis_icd_10_codes)), is_arthritis:=T]


claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), IHD_icd_9_codes)), is_IHD:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), IHD_icd_10_codes)), is_IHD:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), diabetes_icd_9_codes)), is_diabetes:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), diabetes_icd_10_codes)), is_diabetes:=T]


claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), depression_icd_9_codes)), is_depression:=T]

claim_carrier_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
                                                                         ICD_DGNS_CD1,
                                                                         ICD_DGNS_CD2,
                                                                         ICD_DGNS_CD3,
                                                                         ICD_DGNS_CD4,
                                                                         ICD_DGNS_CD5,
                                                                         ICD_DGNS_CD6,
                                                                         ICD_DGNS_CD7,
                                                                         ICD_DGNS_CD8,
                                                                         ICD_DGNS_CD9,
                                                                         ICD_DGNS_CD10,
                                                                         ICD_DGNS_CD11,
                                                                         ICD_DGNS_CD12), depression_icd_10_codes)), is_depression:=T]


head(claim_carrier_all_years[is_hypertension==T & is_IHD==T ])



claim_inpatient_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_inpatient_all_years.fst",as.data.table = T)






claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), hypertension_icd_9_codes)), is_hypertension:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), hypertension_icd_10_codes)), is_hypertension:=T]


claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), arthritis_icd_9_codes)), is_arthritis:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), arthritis_icd_10_codes)), is_arthritis:=T]


claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), IHD_icd_9_codes)), is_IHD:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), IHD_icd_10_codes)), is_IHD:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), diabetes_icd_9_codes)), is_diabetes:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), diabetes_icd_10_codes)), is_diabetes:=T]


claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), depression_icd_9_codes)), is_depression:=T]

claim_inpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,
ADMTG_DGNS_CD,
ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), depression_icd_10_codes)), is_depression:=T]

head(claim_inpatient_all_years[is_hypertension==T & is_IHD==T])



claim_outpatient_all_years=read_fst("/work/postresearch/Shared/Projects/Data_fst/claim_outpatient_all_years.fst",as.data.table = T)



claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), hypertension_icd_9_codes)), is_hypertension:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), hypertension_icd_10_codes)), is_hypertension:=T]


claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), arthritis_icd_9_codes)), is_arthritis:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), arthritis_icd_10_codes)), is_arthritis:=T]


claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), IHD_icd_9_codes)), is_IHD:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), IHD_icd_10_codes)), is_IHD:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), diabetes_icd_9_codes)), is_diabetes:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), diabetes_icd_10_codes)), is_diabetes:=T]


claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 9 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), depression_icd_9_codes)), is_depression:=T]

claim_outpatient_all_years[PRNCPAL_DGNS_VRSN_CD == 0 & Reduce(`|`, Map(chmatch, 
                                                                    list(PRNCPAL_DGNS_CD,

ICD_DGNS_CD1,
ICD_DGNS_CD2,
ICD_DGNS_CD3,
ICD_DGNS_CD4,
ICD_DGNS_CD5,
ICD_DGNS_CD6,
ICD_DGNS_CD7,
ICD_DGNS_CD8,
ICD_DGNS_CD9,
ICD_DGNS_CD10,
ICD_DGNS_CD11,
ICD_DGNS_CD12,
ICD_DGNS_CD13,
ICD_DGNS_CD14,
ICD_DGNS_CD15,
ICD_DGNS_CD16,
ICD_DGNS_CD17,
ICD_DGNS_CD18,
ICD_DGNS_CD19,
ICD_DGNS_CD20,
ICD_DGNS_CD21,
ICD_DGNS_CD22,
ICD_DGNS_CD23,
ICD_DGNS_CD24,
ICD_DGNS_CD25), depression_icd_10_codes)), is_depression:=T]
head(claim_outpatient_all_years[is_hypertension==T & is_IHD==T])









claim_level_all_data=rbind(claim_carrier_all_years[,.(DESY_SORT_KEY,
year,
date,
month_year,
is_hypertension,
is_arthritis,
is_IHD,
is_diabetes,
is_depression)]
,claim_inpatient_all_years[,.(DESY_SORT_KEY,
year,
date,
month_year,
is_hypertension,
is_arthritis,
is_IHD,
is_diabetes,
is_depression)])
claim_level_all_data=rbind(claim_level_all_data
,claim_outpatient_all_years[,.(DESY_SORT_KEY,
year,
date,
month_year,
is_hypertension,
is_arthritis,
is_IHD,
is_diabetes,
is_depression)])

head(claim_level_all_data)









summarise_claim_level <- function(data, time_frame = 365) {
  data %>%
    group_by(DESY_SORT_KEY, year) %>%
    summarise(
      hypertension = sum(is_hypertension, na.rm = T) > 0,
      arthritis = sum(is_arthritis, na.rm = T) > 0,
      IHD = sum(is_IHD, na.rm = T) > 0,
      diabetes = sum(is_diabetes, na.rm = T) > 0,
      depression = sum(is_depression, na.rm = T) > 0,
    ) %>%
    as.data.table()
}

summary_claim_level_patient_by_year <- summarise_claim_level(claim_level_all_data)



tail(summary_claim_level_patient_by_year)


write_fst(summary_claim_level_patient_by_year, "/work/postresearch/Shared/Projects/Farbod/CaseMix/summary_claim_level_patient_by_year_new_new.fst")
