library(purrr)

target_dir <- "football-clubs-voronoi-de/team_icons/"
if (!dir.exists(target_dir)) {
  dir.create(target_dir)
}

icon_urls <- c(
  "FC Bayern Munich" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F14_20170731800.png",
  "Borussia Dortmund" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F17_20150212741.png",
  "Bayer 04 Leverkusen" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2019%2Ffussball%2Fvereine%2Fxxl%2F9_20181114991.png",
  "RB Leipzig" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F15778_20200716872.png",
  "1. FC Union Berlin" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F62_20200812304.png",
  "SC Freiburg" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F7_20150226260.png",
  "1. FC Köln" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F16_20150226713.png",
  "1. FSV Mainz 05" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F30_20200804894.png",
  "TSG Hoffenheim" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2017%2Ffussball%2Fvereine%2Fxxl%2F3209_20160810540.png",
  "Borussia Mönchengladbach" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F15_20160215335.png",
  "Eintracht Frankfurt" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2009%2Ffussball%2Fvereine%2Fxxl%2F32_20150225827.png",
  "VfL Wolfsburg" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2018%2Ffussball%2Fvereine%2Fxxl%2F24_20180318873.png",
  "VfL Bochum"           = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2021%2Ffussball%2Fvereine%2Fxxl%2F8_20200804901.png",
  "FC Augsburg" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2016%2Ffussball%2Fvereine%2Fxxl%2F91_20160502546.png",
  "VfB Stuttgart" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2015%2Ffussball%2Fvereine%2Fxxl%2F11_20150226154.png",
  "Hertha BSC" 	= "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https%3A%2F%2Fsecure-mediadb.kicker.de%2F2013%2Ffussball%2Fvereine%2Fxxl%2F29_20150226842.png",
  "FC Schalke 04" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/2.png",
  "SV Werder Bremen" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/4_20150225702.png"
)

icon_urls <- c(
  "SSV Jahn Regensburg" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2021/fussball/vereine/xxl/1377_20220704177.png",
  "1. FC Kaiserslautern" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2017/fussball/vereine/xxl/37_20161215433.png",
  "SC Paderborn 07" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2018/fussball/vereine/xxl/109_20180329773.png",
  "1. FC Heidenheim" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2018/fussball/vereine/xxl/11306_20170721389.png",
  "Hamburger SV" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2012/fussball/vereine/xxl/12_20200615597.png",
  "Fortuna Düsseldorf" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/13.png",
  "SV Sandhausen" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2020/fussball/vereine/xxl/1604_20200122856.png",
  "F.C. Hansa Rostock" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2011/fussball/vereine/xxl/3_201177154042550.png",
  "SV Darmstadt 98" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2016/fussball/vereine/xxl/98_20150723389.png",
  "Holstein Kiel" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2021/fussball/vereine/xxl/1297_20210310772.png", 
  "1. FC Nürnberg" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2013/fussball/vereine/xxl/81_20150310408.png",
  "FC St. Pauli" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2020/fussball/vereine/xxl/18_20190820714.png",
  "1. FC Magdeburg" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2017/fussball/vereine/xxl/128_20160811101.png",
  "SpVgg Greuther Fürth" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2018/fussball/vereine/xxl/82_20170712237.png",
  "Hannover 96" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2009/fussball/vereine/xxl/58_20150226091.png",
  "Karlsruher SC" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2016/fussball/vereine/xxl/6_20150721308.png",
  "Arminia Bielefeld" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2019/fussball/vereine/xxl/10_20191127726.png",
  "Eintracht Braunschweig" = "https://derivates.kicker.de/image/fetch/w_120,h_120,c_fit,q_auto:best/https://mediadb.kicker.de/2018/fussball/vereine/xxl/41_20190430543.png"
)


download.file(icon_urls, method = "libcurl", mode = "wb", 
              destfile = paste0(target_dir, names(icon_urls), ".png"))

icon_files <- list.files(target_dir, pattern = "*.png")
names(icon_files) <- names(icon_urls)
icon_files

# check of all plots are in png format
walk(file.path(target_dir, icon_files), function(x) {message(x)
  png::readPNG(x)})




