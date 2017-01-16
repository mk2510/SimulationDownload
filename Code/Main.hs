module Main (main) where

--import SimulationFunction
import SimuGUI

--Gui import
import Graphics.Gloss


-- evtl wegen volumen suszeptinilität prüfen - formelsammlung
getCHI:: Int -> Double
getCHI element
        | (element == 3) = 1.00002 -1
        | (element == 4) = 1.00028 -1
        | (element == 5) = 1.0002 -1
        | (element == 1) = 0.999884 -1
        | (element == 2) = 0.999971 -1
        | (element == 6) = 0.999986 -1

-- periodensystem clemens -volumen in cm^3 - masse in g
getMass::Int -> Double -> Double
getMass element r
          | (element == 3) = 2.70 * (getVolumen r)
          | (element == 4) = 7.20 * (getVolumen r)
          | (element == 5) = 21.4 * (getVolumen r)
          | (element == 1) = 6.68 * (getVolumen r)
          | (element == 2) = 19.32  * (getVolumen r)
          | (element == 6) = 7.14 * (getVolumen r)

getColor::Int -> Color
getColor element
          | (element == 3) = greyN 0.6
          | (element == 4) = greyN 0.65
          | (element == 5) = greyN 0.7
          | (element == 1) = greyN 0.5
          | (element == 2) = dim yellow
          | (element == 6) = greyN 0.55


--volumen einer kugel mit radius r
getVolumen:: Double -> Double
getVolumen r = (4/3) * (r^3) * pi

convertGIntoKG :: Double -> Double
convertGIntoKG g = g * (1/1000)

convertCMIntoMeter:: Double -> Double
convertCMIntoMeter g = g* (1/100)

main :: IO()
main = gui

gui :: IO()
gui = do
      putStrLn("--------------------------------------------------------------------")
      putStrLn("------Simulation zu der Seminararbeit Dia- und Paramagnetismus------")
      putStrLn("--------------------------------------------------------------------")
      putStrLn("----------------------------Maximilian Krahn------------------------")
      putStrLn("--------------------------------------------------------------------")
      putStrLn("Simulationsdaten:---------------------------------------------------")
      putStrLn("                - Temperatur: 293,15 K")
      putStrLn("                - Länge des Fadens: 400 m")
      putStrLn("--------------------------------------------------------------------")
      putStrLn("Hinweise zu den Paramatereingaben:----------------------------------")
      putStrLn("                                  - Kugelradius: 0.3 cm - 4.9 cm")
      putStrLn("                                  - Magnetfeldstaerke: 0.01 T - 25 T")
      putStrLn("--------------------------------------------------------------------")
      putStrLn("------------------------Start der Parametereingabe:-----------------")
      putStrLn("Auswahl des Kugelradius in cm:")
      radi <- getLine
      putStrLn("Auswahl der Staerke des Magnetfeldes in T (beim Erzeuger):")
      bF <- getLine
      putStrLn("Auswahl des Elementes:")
      putStrLn("Antimon = 1, Gold = 2, Aluminium = 3, Chrom = 4,  Platin = 5, Zink = 6")
      elem <- getLine

      startGui (read bF ::Double) (getCHI (read elem:: Int)) (getMass (read elem ::Int)(read radi::Double)) (convertCMIntoMeter(read radi ::Double))(getColor(read elem ::Int))

      putStrLn("---Druecken Sie 'Enter' um die Parametereingabe erneut zu starten---")
      enter <- getLine
      main


{--
    initGUI
    window <- windowNew
    vbox <- vBoxNew True 20
    hbox0 <- hBoxNew True 20
    hbox1 <- hBoxNew True 20
    hbox2 <- hBoxNew True 20
    hbox3 <- hBoxNew True 20

    buttonStart <- buttonNewWithLabel "Start"

    labelRad<- labelNew (Just "Radius in cm:")
    labelElem<- labelNew (Just "Auswahl des Elementes aus: Antimon, Gold, Aluminium, Chrom,  Platin")
    labelMagn <- labelNew (Just "Magnetfeldstaerke in T:")

    txtRad <- entryNew
    txtMagn <- entryNew
    txtElem <- entryNew
--}

    --lines for the combo box, which stores the list of elements
    {--store <- listStoreNew ["Antimon", "Gold", "Quecksilber", "Aluminium", "Chrom",  "Platin"]
    comboElem <- comboBoxNewWithModel store
    ren <- cellRendererTextNew
    cellLayoutPackEnd comboElem ren True
    cellLayoutSetAttributes comboElem ren store
      (\txt -> [cellText := txt])
        --}
    --setting the window variabls
    {--set window [windowDefaultWidth := 200,
                windowDefaultHeight := 200,
                containerBorderWidth := 10,
                windowTitle := "Parametereingabe",
                containerChild := vbox]



    --packing single boxes with the gui stuff
    boxPackStart hbox0 labelRad PackGrow 0
    boxPackStart hbox1 txtRad PackGrow 0

    boxPackStart hbox1 labelMagn PackGrow 0
    boxPackStart hbox1 txtMagn PackGrow 0

    boxPackStart hbox2 labelElem PackGrow 0
    boxPackStart hbox2 txtElem PackGrow 0

    boxPackStart hbox3 buttonStart PackGrow 0

    --packing the single boxes to the whole one
    boxPackStart vbox hbox0 PackGrow 0
    boxPackStart vbox hbox1 PackGrow 0
    boxPackStart vbox hbox2 PackGrow 0
    boxPackStart vbox hbox3 PackGrow 0

    --magnefeld <- unsafePerformIO $ entryGetText txtMagn


    onClicked buttonStart $ startGui (unsafePerformIO (entryGetText txtMagn) ) (getCHI (unsafePerformIO (entryGetText txtElem))) (convertGIntoKG(getMass(unsafePerformIO (entryGetText txtElem))(unsafePerformIO (entryGetText txtRad)) )) (unsafePerformIO (entryGetText txtRad))


    --magnet_feld_staerke = read (entryGetText txtMagn) ::Integer
    --temperatur = read (entryGetText txtTemp) :: Integer
     --comboBoxGetActiveText

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI--}
