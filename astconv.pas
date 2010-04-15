{$DEFINE Kopro}
UNIT AstConv;


{
  **************************************************************************
  ***                                                                    ***
  ***                       A S T C O N V . P A S                        ***
  ***                                                                    ***
  ***                 (c) 1990,91,92 by Wolfram Schroers                 ***
  ***                                                                    ***
  **************************************************************************

  Diese UNIT stellt das HerzstÅck von AstroCAD dar; sie konvertiert die
  wesentlichen Formeln und astronom. Grî·en untereinander. Sie stellt nicht
  nur die meisten math. Grundlagen (der Rest ist in Vectors) sondern auch
  die verwaltungstechnischen Grundlagen (auf die AstVerw aufbaut) zur Ver-
  fÅgung. Inzwischen liegt sie in der Version 0.9a Beta vor, so da· sie
  wesentlich mehr kann, als ursprÅnglich geplant!

  Version:0.9a Beta
}


{$IFDEF Kopro}
{$N+,E+}
{$ELSE}
{$N-,E-}
{$ENDIF}
{S-,R-}



INTERFACE


USES Crt,Vectors;


CONST Zone        = 1;                  { Zeitzone; eine Konstante fÅr MEZ }
      SekproTag   = 86400;              { Sekunden pro Sonnentag }
      SichtHimBr  = 100;                { Breite des sichtbaren Himmelsausschnittes }
      MaxKoerper  = 20;                 { Max. Anzahl Kîrper in der Sonnensystemsimulation, die
                                          gleichzeitig berÅcksichtigt werden kînnen }
      f:Skalar    = 6.672E-11;          { Gravitationskonstante }
      c:Skalar    = 299792458;          { Vakuumlichtgeschwindigkeit in m/s }
      Erd_g       = 9.806;              { Beschleunigung an ErdoberflÑche in m/s^2 }
      AstroUnit:Skalar
                  = 1.4959787066E11;    { Astronomische Einheit (in m) }
      ErdRadius   = 6378137;            { Erdradius am équator (in m) }
      MondRadius  = 1738000;            { Mittlerer Mondradius (in m) }
      SonnenMasse:Skalar
                  = 1.9891E30;          { Masse der Sonne (in kg) }
      ErdMasse:Skalar
                  = 1.9891E30/332946.038;
                                        { Masse der Erde (in kg) }
      MondErdeMassenVerhaeltnis:Skalar
                  = 0.012300034;        { MondMasse:=MEMV*ErdMasse }
      PlanetenMasse:ARRAY[1..11] OF Skalar
                  = (      1.0,
                      332946.038,
                           0.012300034,
                     6023600.0,
                      408523.5,
                     3098710.0,
                        1047.355,
                        3498.5,
                       22869.0,
                       19314.0,
                     3000000.0 );       { tats. PMasse:=SonnenMasse/PlanetenMasse }
      PRadius     : ARRAY[1..11] OF Skalar
                  = ( 696000000.0,
                       ErdRadius,
                      MondRadius,
                        2439000.0,
                        6052000.0,
                        3397200.0,
                       71398000.0,
                       60000000.0,
                       25400000.0,
                       24300000.0,
                        2500000.0 );    { Kîrperradius in m }
      Aktiv       = TRUE;               { Status eines Kîrpers (z.Z. in TSS-Feld) }
      NichtAktiv  = FALSE;              { Kîrper z.Z. nicht verwendbar }
      Tage_pro_Monat : ARRAY[1..12] OF Byte
                     = ( 31, 28, 31, 30, 31, 30,
                         31, 31, 30, 31, 30, 31 );


TYPE  TKoerperTyp = (Sonne,Planet,Mond,Asteroid,Komet);
      PSSystem    = ^TSSystem;          { Zeiger auf TSSystem }
      PBKoerper   = ^TBKoerper;         { Zeiger auf Beschreibung des Kîrpers }
      PRKInfo     = ^TRKInfo;           { Zeiger auf Daten des RK-Verfahrens }

      THKoerper   = RECORD              { Himmelskîrper (Planet, Mond etc.) }
                      Posi:TVector;     { Positionsvektor im Raum (in m) }
                      Velo:TVector;     { Geschwindigkeitsvektor (in m/s) }
                      Masse:Skalar;     { Masse des jeweiligen Himmelskîrpers in kg }
                      UpDate:Boolean;   { Soll der Kîrper selbst aktualisiert werden ? }
                      Status:Boolean;   { Soll der Kîrper Åberhaupt in Rechnungen verwendet
                                          werden ? (Aktiv/NichtAktiv) }
                      Info:PBKoerper    { Informationen Åber den Kîrper (auf dem Heap) }
                    END;
      TBKoerper   = RECORD              { Himmelskîrper als Beschreibungsdatei (nicht fÅr Iteration) }
                      Beschreib:String; { Kurze Textbeschreibung des Kîrpers, zur Identifizierung und
                                          Katalogisierung }
                      Descrip:Integer;  { I/O-Deskriptor, der nur das Ein-/Ausgabeformat des
                                          jeweiligen Kîrpers beschreibt (siehe TSS-Spezifik.) }
                      KoerperTyp:TKoerperTyp; { Ist der Kîrper Sonne, Planet, Mond oder sonstiges ? }
                      ZentKoerp:Byte    { Nummer des zugehîrigen Zentralkîrpers }
                    END;
      TSSystem    = ARRAY[1..MaxKoerper] OF THKoerper;
      TRegCallProc = PROCEDURE (VAR AnzK:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR Infos:Pointer);
      TVecArray   = ARRAY[1..MaxKoerper] OF TVector;
      TRKInfo     = RECORD              { Informationsrecord fÅr Runge-Kutta-Verfahren }
                      AnzK:Byte;        { Anzahl der Kîrper }
                      Koerper:TSSystem; { Die Koerper selbst }
                      Faktor:Skalar;    { Vielfaches in Sekunden (Iterationsschrittweite h) }
                      Tage:Skalar;      { Anzahl der Tage, die das Verfahren durcharbeiten mu·;
                                          enthÑlt nach Beendigung den noch nicht iterierten
                                          öberrest in Sekunden (bei autom. Schrittweitenkorr. oder
                                          n*Faktor<>Tage) }
                      I2:LongInt;       { Aufruf der Prozedur RegCall (TRegCallProc) alle I2 Schritte }
                      RegCall:TRegCallProc; { s.o. I2 }
                      Infos:Pointer     { Informationen fÅr RegCall }
                    END;
      TGSInfo     = RECORD              { Informationen fÅr GetSpeed }
                      AnzK:Byte;                { Anzahl der Kîrper }
                      Koerper:TSSystem; { Die Koerper selbst }
                      Faktor:Skalar;    { Iterationsschrittweite in Sekunden }
                      Weite:LongInt;    { Anzahl der Iterationsschritte zwischen Start und Ziel }
                      Start,Ziel:TVecArray; { Start- und Zielkoordinaten }
                      Alpha:Skalar;     { BerÅcksichtigung des Neu-Gelernten }
                      Eta:Skalar;       { BerÅcksichtigung des vergangenen Lernprozesses }
                      AktFehler:Skalar  { Jeweiliger Abbruchfehler (zurÅckgegeben wird der letzte Wert }
                    END;
      TDarkness   = (Keine,SonF,MondF); { Ist die Finsternis eine Sonne- oder Mondfinsternis ? }



{
  Globale Variablendeklarationen: kînnen von AstroCAD variiert werden
}


VAR     SonneNr,ErdNr,MondNr,MarsNr,SaturnNr,PlutoNr:Byte;


{
  Erster Teil: Grundlegende math. Elemente; simple Umrechnungen
}

  FUNCTION Sgn(x:Skalar):Skalar;
   { Ermittelt das Vorzeichen einer Variable }

  FUNCTION Tan(x:Skalar):Skalar;
   { Ergibt den Tangens von x }

  FUNCTION Cot(x:Skalar):Skalar;
   { Ergibt den Kotangens von x }

  FUNCTION ArcSin(x:Skalar):Skalar;
   { Ergibt den Arkussinus von x }

  FUNCTION ArcCos(x:Skalar):Skalar;
   { Ergibt den Arkuscosinus von x }

  FUNCTION ArcCot(x:Skalar):Skalar;
   { Ergibt den Arkuscotangens von x }

  FUNCTION ArcSec(x:Skalar):Skalar;
   { Ergibt den Arkussekans von x }

  FUNCTION ArcCoSec(x:Skalar):Skalar;
   { Ergibt den Arkuscosekans von x }

  FUNCTION Sinh(x:Skalar):Skalar;
   { Ergibt den Sinus-Hyperbolikus von x }

  FUNCTION Cosh(x:Skalar):Skalar;
   { Ergibt den Cosinus-Hyperbolikus von x }

  FUNCTION Tanh(x:Skalar):Skalar;
   { Ergibt den Tangens-Hyperbolikus von x }

  FUNCTION Coth(x:Skalar):Skalar;
   { Ergibt den Cotangens-Hyperbolikus von x }

  FUNCTION Sech(x:Skalar):Skalar;
   { Ergibt den Sekans-Hyperbolikus von x }

  FUNCTION CoSech(x:Skalar):Skalar;
   { Ergibt den Cosekans-Hyperbolikus von x }

  FUNCTION ArcSinh(x:Skalar):Skalar;
   { Ergibt den Arkussinus-Hyperbolikus von x }

  FUNCTION ArcCosh(x:Skalar):Skalar;
   { Ergibt den Arkuscosinus-Hyperbolikus von x }

  FUNCTION ArcTanh(x:Skalar):Skalar;
   { Ergibt den Arkustangens-Hyperbolikus von x }

  FUNCTION ArcCoth(x:Skalar):Skalar;
   { Ergibt den Arkuscotangens-Hyperbolikus von x }

  FUNCTION ArcSech(x:Skalar):Skalar;
   { Ergibt den Arkussekans-Hyperbolikus von x }

  FUNCTION ArcCoSech(x:Skalar):Skalar;
   { Ergibt den Arkuscosekans-Hyperbolikus von x }

  FUNCTION Power(x,y:Skalar):Skalar;
   { Liefert x^y in Power zurÅck }

  PROCEDURE StrtoDeg(Zeile:String;VAR h,m,s:Skalar);
   { Wandelt eine Zeile in Grad, Minuten und Sekunden um }

  FUNCTION DegtoRad(x:Skalar):Skalar;
   { Konvertiert einen Winkel im Gradma· in das Bogenma· }

  FUNCTION RadtoDeg(x:Skalar):Skalar;
   { Macht das Gegenteil wie DegtoRad }

  FUNCTION RAtoDeg(h,m,s:Skalar):Skalar;
   { Konvertiert die Rektaszension aus der 24h-Darstellung ins Bogenma· }

  PROCEDURE DegtoRA(x:Skalar;VAR h,m,s:Skalar);
   { Konvertiert die Rektaszension aus einem Bogm. Winkel in die 24h-Darst. }

  FUNCTION DegtoDeg(h,m,s:Skalar):Skalar;
   { Konvertiert einen Winkel im Bogenmin. und Sek.-Format in eine Dezimalzahl }

  PROCEDURE DegtoHMS(x:Skalar;VAR h,m,s:Skalar);
   { Kehrt die Funktion DegtoDeg um, so da· man das Ausgangsformat erhÑlt }

  FUNCTION Obliq(JD:Skalar):Skalar;
   { Winkel Ekliptik-HimmelsÑquator aus dem Julianischen Datum bilden }

{
  Zweiter Teil: Berechnungen zur Darstellung
}

  FUNCTION Zeit(Stunden,Minuten,Sekunden,Sek100:Word):Skalar;
   { Konvertiert die Uhrzeit in eine Skalar-Variable }

  PROCEDURE UhrZeit(Zeit:Skalar;VAR Hour,Min,Sec:Integer);
   { Konvertiert die Skalarvariable wieder zurÅck zum Uhrzeitformat }

  FUNCTION MEZtoMOZ(MEZ,Laenge:Skalar):Skalar;
   { Konvertiert die Zonenzeit in die mittl. Ortszeit. Benîtigt geogr. LÑnge }

  FUNCTION MEZtoUT(MEZ:Skalar):Skalar;
   { Konvertiert die Zonenzeit in die Weltzeit }

  FUNCTION LightTime(VAR Koerper:TSSystem;KNr1,KNr2:Byte;h:Skalar):Integer;
   { Liefert die Anzahl Iterationsschritte, die das Licht zum Durchlaufen
     der Strecke zwischen KNr1 und KNr2 braucht (h=Iterationsschrittweite) }

  PROCEDURE JDtoUT(JulDat:Skalar;VAR Tag,Monat,Jahr,Stunden,Minuten:Integer);
   { Liefert das interpretierte Julianische Datum zurÅck }

  PROCEDURE MOZtoSRT(MOZ:Skalar;Tag,Monat,Jahr:Integer;VAR JulDat,SRT:Skalar);
   { Konvertiert die Ortszeit in die Sternzeit und Julianische Zeit }

  PROCEDURE RaDtoAzH(Rekta,Dekl:Skalar;SRT,Breite:Skalar;VAR Azimuth,Hoehe:Skalar);
   { Konvertiert HimmelsÑquatorkoordinaten in das Horizontsystem }

  PROCEDURE RaDtoELB(Rek,Dekl:Skalar;VAR ELaenge,EBreite:Skalar;JD:Skalar);
   { Konvertiert die Rektaszension und Deklination in Eklip. Laenge und Breite }

  PROCEDURE ELBtoRaD(ELaenge,EBreite:Skalar;VAR Rek,Dekl:Skalar;JD:Skalar);
   { Konvertiert die Eklip. Laenge und Breite nach Rektaszension und Deklination }

{
  Dritter Teil: Berechnungen in einem 3D-Koordinatensystem
}

  FUNCTION Parallax(VAR Koerper:TSSystem;KNr:Byte):Skalar;
   { Liefert die geozentr., TéGLICHE Parallaxe eines Kîrpers in Grad }

  FUNCTION Semidiam(VAR Koerper:TSSystem;KNr:Byte):Skalar;
   { Der geozentr. scheinbare Radius eines Kîrpers in Grad }

  FUNCTION Elongation(VAR Koerper:TSSystem;KNr:Byte):Skalar;
   { Die geozentr. Elongation eines Kîrpers (Kîrper/Sonne Winkelabstand) }

  PROCEDURE RaDtoVec(Rek,Dekl,Dist:Skalar;VAR x,y,z:Skalar;JD:Skalar);
   { Verwandelt Rektaszension und Deklination in 3D-Koordinaten }

  PROCEDURE ELBtoVec(ELaenge,EBreite,Dist:Skalar;VAR x,y,z:Skalar);
   { Wie RaDtoVec, nur das EL und EB nicht vorher umgerechnet werden mÅssen }

  PROCEDURE VectoELB(x,y,z:Skalar;VAR ELaenge,EBreite,Dist:Skalar);
   { Kehrt ELBtoVec um; man erhÑlt nun EL und EB direkt }

  PROCEDURE RungeKutta(VAR Data:TRKInfo);
   { Verbessertes Verfahren, sollte immer anstatt von Iteration verwendet
     werden. Die DatenÅbergabe erfolgt Åber ein Record (s.o.) }

  PROCEDURE DiffMatrix(VAR Data:TRKInfo;Matrix:TMatrix;MDim:Byte);
   { Lîst das System von Differentialgleichungen mit einem Verfahren, dessen
     Matrix Åbergeben wird. Die letzte Zeile in Matrix (MDim+1) gibt
     Åbrigens die Gewichtung der Korrekturfaktoren an. Die Matrix kann
     nicht als VAR-Parameter Åbergeben werden, da sie zerstîrt wird. }

  PROCEDURE Dummy(VAR AnzK:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR Infos:Pointer);
   { Prozedur fÅr Iteration, die die bedeutende Aufgabe hat, nichts zu tun! }

  PROCEDURE GetSpeed(VAR Data:TGSInfo);
   { Prozedur zur Ermittlung der Geschwindigkeitsvektoren, wenn die Ortsvektoren
     gegeben sind. Dazu sind im Feld Start zunÑchst die Startposition der
     Kîrper anzugeben, dann der folgende Punkt (Weite gibt die Zeitdifferenz
     hierzu in Sekunden an). Die Optimierung wird solange durchgefÅhrt, bis
     eine Taste gedrÅckt wird, oder sich der Fehler wiederholt hat. Die
     Suche wird nur fÅr Kîrper durchgefÅhrt, deren UpDate-Wert auf TRUE
     gesetzt wurde! }

  FUNCTION EKin(Nummer:Byte;VAR Koerper:TSSystem):Skalar;
   { Ermittelt die kinetische Energie eines Kîrpers des Systems
     (in kg*m^2/s^2) }

  FUNCTION EPot(Nummer,AnzK:Byte;VAR Koerper:TSSystem):Skalar;
   { Ermittelt die potentielle Energie eines Kîrpers des Systems (sonst
     wie EKin) }

  FUNCTION Energie(AnzK:Byte;VAR Koerper:TSSystem):Skalar;
   { Berechnet die Gesamtenergie (kinetisch+potentiell), die das System
     besitzt. (Greift auf EKin und EPot zurÅck!) }

  PROCEDURE Schwerpunkt(AnzK:Byte;VAR Koerper:TSSystem;VAR SPunkt:TVector);
   { Ermittelt den Massenmittelpunkt des Systems }

  PROCEDURE Eclipse(AnzK:Byte;VAR Koerper:TSSystem;VAR FAbst,halb,voll:Skalar;
                    VAR FTyp:TDarkness);
   { Untersucht, ob eine Finsternis aufgrund der Konstellationen eingetreten
     ist; die Art der Finsternis wird separat bestimmt; folgende Informationen
     werden fÅr diesen Zweck zurÅckgeliefert:
       FAbst : Abstand der Mittelpunkte voneinander
       halb,voll : Halb- und Totalschattenradius des bedeckenden Kîrpers
       FTyp : Prinzipiell Sonnen- oder Mondfinsternis ? }


IMPLEMENTATION

  FUNCTION Sgn(x:Skalar):Skalar;
  BEGIN
    IF x>0 THEN Sgn:=1 ELSE IF x<0 THEN Sgn:=-1 ELSE Sgn:=0
  END;

  FUNCTION Tan(x:Skalar):Skalar;
  BEGIN
    Tan:=Sin(x)/Cos(x)
  END;

  FUNCTION Cot(x:Skalar):Skalar;
  BEGIN
    Cot:=1/Tan(x)
  END;

  FUNCTION ArcSin(x:Skalar):Skalar;
  BEGIN
    IF x=1 THEN ArcSin:=0 ELSE IF x=-1 THEN ArcSin:=-Pi/2
      ELSE ArcSin:=ArcTan(x/Sqrt(1-x*x))
  END;

  FUNCTION ArcCos(x:Skalar):Skalar;
  BEGIN
    IF x=1 THEN ArcCos:=Pi/2 ELSE IF x=-1 THEN ArcCos:=Pi
      ELSE ArcCos:=-ArcTan(x/Sqrt(1-x*x))+Pi/2
  END;

  FUNCTION ArcCot(x:Skalar):Skalar;
  BEGIN
    ArcCot:=Pi/2-ArcTan(x)
  END;

  FUNCTION ArcSec(x:Skalar):Skalar;
  BEGIN
    ArcSec:=ArcTan(Sqrt(x*x-1))+(Sgn(x)-1)*Pi/2
  END;

  FUNCTION ArcCoSec(x:Skalar):Skalar;
  BEGIN
    ArcCoSec:=ArcTan(1/Sqrt(x*x-1))+(Sgn(x)-1)*Pi
  END;

  FUNCTION Sinh(x:Skalar):Skalar;
  BEGIN
    Sinh:=(Exp(x)-Exp(-x))/2
  END;

  FUNCTION Cosh(x:Skalar):Skalar;
  BEGIN
    Cosh:=(Exp(x)+Exp(-x))/2
  END;

  FUNCTION Tanh(x:Skalar):Skalar;
  BEGIN
    Tanh:=-2*Exp(-x)/(Exp(x)+Exp(-x))+1
  END;

  FUNCTION Coth(x:Skalar):Skalar;
  BEGIN
    Coth:=2*Exp(-x)/(Exp(x)-Exp(-x))+1
  END;

  FUNCTION Sech(x:Skalar):Skalar;
  BEGIN
    Sech:=2/(Exp(x)+Exp(-x))
  END;

  FUNCTION CoSech(x:Skalar):Skalar;
  BEGIN
    CoSech:=2/(Exp(x)-Exp(-x))
  END;

  FUNCTION ArcSinh(x:Skalar):Skalar;
  BEGIN
    ArcSinh:=Ln(x+Sqrt(x*x+1))
  END;

  FUNCTION ArcCosh(x:Skalar):Skalar;
  BEGIN
    ArcCosh:=Ln(x+Sqrt(x*x-1))
  END;

  FUNCTION ArcTanh(x:Skalar):Skalar;
  BEGIN
    ArcTanh:=Ln((1+x)/(1-x))/2
  END;

  FUNCTION ArcCoth(x:Skalar):Skalar;
  BEGIN
    ArcCoth:=Ln((x+1)/(x-1))/2
  END;

  FUNCTION ArcSech(x:Skalar):Skalar;
  BEGIN
    ArcSech:=Ln(1+Sqrt(1-x*x))/x
  END;

  FUNCTION ArcCoSech(x:Skalar):Skalar;
  BEGIN
    ArcCoSech:=Ln((Sgn(x)*Sqrt(x*x+1)+1)/x)
  END;

  FUNCTION Power(x,y:Skalar):Skalar;
  BEGIN
    Power:=Exp(y*Ln(x))
  END;

  PROCEDURE StrtoDeg(Zeile:String;VAR h,m,s:Skalar);
    FUNCTION Leer_entf(Zeile:String):String;
    BEGIN
      WHILE Zeile[1]=' ' DO Delete(Zeile,1,1);
      Leer_entf:=Zeile
    END;
    PROCEDURE Naechster(VAR Zeile:String;VAR Wert:Skalar);
    VAR Hilf:Byte;
        Code:Integer;
        HZeil:String;
    BEGIN
      Zeile:=Leer_entf(Zeile);
      Hilf:=Pos(' ',Zeile);
      HZeil:=Copy(Zeile,1,Hilf-1); Delete(Zeile,1,Hilf);
      Val(HZeil,Wert,Code)
    END;
  BEGIN
    Zeile:=ConCat(Zeile,' ');
    WHILE Pos(',',Zeile)>0 DO Zeile[Pos(',',Zeile)]:='.';
    Naechster(Zeile,h);
    Naechster(Zeile,m);
    Naechster(Zeile,s)
  END;

  FUNCTION DegtoRad(x:Skalar):Skalar;
  BEGIN
    DegtoRad:=x/180*Pi
  END;

  FUNCTION RadtoDeg(x:Skalar):Skalar;
  BEGIN
    RadtoDeg:=x*180/Pi
  END;

  FUNCTION RAtoDeg(h,m,s:Skalar):Skalar;
  BEGIN
    RAtoDeg:=h*15+m*0.25+s*3/720
  END;

  PROCEDURE DegtoRA(x:Skalar;VAR h,m,s:Skalar);
  BEGIN
    h:=Int(x/15);
    m:=Int((x-h*15)*4);
    s:=(x-h*15-m*0.25)*240
  END;

  FUNCTION DegtoDeg(h,m,s:Skalar):Skalar;
  BEGIN
    DegtoDeg:=h+m/60+s/3600
  END;

  PROCEDURE DegtoHMS(x:Skalar;VAR h,m,s:Skalar);
  BEGIN
    h:=Int(x);
    m:=Int((x-h)*60);
    s:=(x-h-m/60)*3600
  END;

  FUNCTION Obliq(JD:Skalar):Skalar;
  VAR T:Skalar;
  BEGIN
    T:=(JD-2451545)/36525;
    Obliq:=23.439291-0.0130042*T-0.00000016*T*T+0.000000504*T*T*T
  END;

  FUNCTION Zeit(Stunden,Minuten,Sekunden,Sek100:Word):Skalar;
  BEGIN
    Zeit:=Stunden+Minuten/60+(Sekunden+Sek100/100)/3600
  END;

  PROCEDURE UhrZeit(Zeit:Skalar;VAR Hour,Min,Sec:Integer);
  BEGIN
    Hour:=Trunc(Zeit);
    Min:=Trunc((Zeit-Hour)*60);
    Sec:=Trunc((Zeit-Hour-Min/60)*3600)
  END;

  FUNCTION MEZtoMOZ(MEZ,Laenge:Skalar):Skalar;
  BEGIN
    MEZtoMOZ:=(MEZ-Zone)+(Laenge/15)
  END;

  FUNCTION MEZtoUT(MEZ:Skalar):Skalar;
  BEGIN
    MEZtoUT:=MEZ-Zone
  END;

  FUNCTION LightTime(VAR Koerper:TSSystem;KNr1,KNr2:Byte;h:Skalar):Integer;
  VAR Hilf:TVector;
  BEGIN
    VSub(Hilf,Koerper[KNr1].Posi,Koerper[KNr2].Posi);
    LightTime:=Round(VLen(Hilf)/c/h)
  END;

  FUNCTION Days_per_year(Y:Integer):Integer; {private}
  BEGIN
    IF Y MOD 100=0 THEN Days_per_year:=365
      ELSE IF Y MOD 4=0 THEN Days_per_year:=366 ELSE Days_per_year:=365
  END;

  FUNCTION Days_per_month(Y,M:Integer):Integer; {private}
  BEGIN
    IF M<>2 THEN Days_per_month:=Tage_pro_Monat[M]
      ELSE IF Days_per_year(Y)=366 THEN Days_per_month:=29
        ELSE Days_per_month:=28
  END;

  FUNCTION This_day_number(D,M,Y:Integer):Integer; {private}
  VAR Hilf:Integer;
      Lauf:Byte;
  BEGIN
    Hilf:=0;
    FOR Lauf:=1 TO M DO Hilf:=Hilf+Tage_pro_Monat[Lauf];
    IF (M>2) AND (Days_per_year(Y)=366) THEN Inc(Hilf);
    This_day_number:=Hilf+D
  END;

  PROCEDURE JDtoUT(JulDat:Skalar;VAR Tag,Monat,Jahr,Stunden,Minuten:Integer);
  CONST JD1991:Skalar=2448256.5;
  VAR JDakt:Skalar;
      Hilf:Integer;
  BEGIN
    Jahr:=1991;
    JDakt:=JD1991;
    WHILE (JDakt+Days_per_year(Jahr)<JulDat) DO
    BEGIN JDakt:=JDakt+Days_per_year(Jahr); Inc(Jahr) END;
    WHILE (JDAkt-Days_per_year(Jahr-1)>JulDat) DO
    BEGIN JDakt:=JDakt-Days_per_year(Jahr); Dec(Jahr) END;
    JulDat:=JulDat-JDakt;
    Monat:=1;
    WHILE JulDat>Days_per_month(Jahr,Monat) DO
    BEGIN
      JulDat:=JulDat-Days_per_month(Jahr,Monat); Inc(Monat)
    END;
    Tag:=Trunc(JulDat); JulDat:=Frac(JulDat);
    Stunden:=Trunc(JulDat*24); JulDat:=JulDat-Stunden/24;
    Minuten:=Trunc(JulDat*1440)
  END;

  PROCEDURE MOZtoSRT(MOZ:Skalar;Tag,Monat,Jahr:Integer;VAR JulDat,SRT:Skalar);
  VAR Ja,Da,Schalt:Integer;
      Tage,TagInsg,Zo,JulTag,StundWink:Skalar;
  BEGIN
    Ja:=Jahr-1900; Schalt:=(Ja-1) DIV 4;
    Tage:=Schalt+Trunc(30.6*Monat+0.53/((Monat-1.55)*(Monat-1.55))-32.3)+Tag-0.5;
    IF (Ja/4-Trunc(Ja/4)=0) AND (Monat>2) THEN Tage:=Tage+1;
    TagInsg:=Ja*365.0+Tage;
    JulDat:=TagInsg+2415020;
    Zo:=6.59731139+6.57098224E-02*(TagInsg-364.5);
    StundWink:=Zo-Trunc(Trunc(Zo/24)*24);
    SRT:=StundWink+MOZ; IF SRT>24 THEN SRT:=SRT-24
  END;

  PROCEDURE RaDtoAzH(Rekta,Dekl:Skalar;SRT,Breite:Skalar;VAR Azimuth,Hoehe:Skalar);
  VAR SWGrad:Skalar;
  BEGIN
    SWGrad:=SRT*15-Rekta;
    SWGrad:=DegtoRad(SWGrad); Breite:=DegtoRad(Breite); Dekl:=DegtoRad(Dekl);
    Hoehe:=ArcSin(Sin(Breite)*Sin(Dekl)+Cos(Breite)*Cos(Dekl)*Cos(SWGrad));
    Azimuth:=ArcCos((Sin(Dekl)*Cos(Breite)-Cos(Dekl)*Cos(SWGrad)*Sin(Breite))/Cos(Hoehe));
    Azimuth:=RadtoDeg(Azimuth); Hoehe:=RadtoDeg(Hoehe);
    SWGrad:=RadtoDeg(SWGrad);
    IF (SWGrad<0) AND (SWGrad>-180) THEN Azimuth:=360-Azimuth;
    IF SWGrad>180 THEN Azimuth:=360-Azimuth
  END;

  PROCEDURE RaDtoELB(Rek,Dekl:Skalar;VAR ELaenge,EBreite:Skalar;JD:Skalar);
  VAR i:Skalar;
  BEGIN
    i:=DegtoRad(Obliq(JD));
    Rek:=DegtoRad(Rek); Dekl:=DegtoRad(Dekl);
    EBreite:=ArcSin(Sin(Dekl)*Cos(i)-Cos(Dekl)*Sin(i)*Sin(Rek));
    ELaenge:=RadtoDeg(ArcCos(Cos(Rek)*Cos(Dekl)/Cos(EBreite)));
    IF Rek>Pi THEN ELaenge:=360-ELaenge;
    EBreite:=RadtoDeg(EBreite)
  END;

  PROCEDURE ELBtoRaD(ELaenge,EBreite:Skalar;VAR Rek,Dekl:Skalar;JD:Skalar);
  VAR i:Skalar;
  BEGIN
    i:=DegtoRad(-Obliq(JD));
    ELaenge:=DegtoRad(ELaenge); EBreite:=DegtoRad(EBreite);
    Dekl:=ArcSin(Sin(EBreite)*Cos(i)-Cos(EBreite)*Sin(i)*Sin(ELaenge));
    Rek:=RadtoDeg(ArcCos(Cos(EBreite)*Cos(ELaenge)/Cos(Dekl)));
    IF ELaenge>Pi THEN Rek:=360-Rek;
    Dekl:=RadtoDeg(Dekl)
  END;

  FUNCTION Parallax(VAR Koerper:TSSystem;KNr:Byte):Skalar;
  VAR Hilf:TVector;
  BEGIN
    VSub(Hilf,Koerper[ErdNr].Posi,Koerper[KNr].Posi);
    Parallax:=RadtoDeg(ArcSin(ErdRadius/VLen(Hilf)))
  END;

  FUNCTION Semidiam(VAR Koerper:TSSystem;KNr:Byte):Skalar;
  VAR Hilf:TVector;
  BEGIN
    VSub(Hilf,Koerper[ErdNr].Posi,Koerper[KNr].Posi);
    Semidiam:=RadtoDeg(ArcSin(PRadius[KNr]/VLen(Hilf)))
  END;

  FUNCTION Elongation(VAR Koerper:TSSystem;KNr:Byte):Skalar;
  VAR Hilf,Hilf2:TVector;
  BEGIN
    VSub(Hilf,Koerper[KNr].Posi,Koerper[ErdNr].Posi);
    VSub(Hilf2,Koerper[SonneNr].Posi,Koerper[ErdNr].Posi);
    VecNorm(Hilf); VecNorm(Hilf2);
    Elongation:=RadtoDeg(ArcCos(Abs(VSProd(Hilf,Hilf2))))
  END;

  PROCEDURE RaDtoVec(Rek,Dekl,Dist:Skalar;VAR x,y,z:Skalar;JD:Skalar);
  VAR ELaenge,EBreite:Skalar;
  BEGIN
    RaDtoELB(Rek,Dekl,ELaenge,EBreite,JD);
    ELBtoVec(ELaenge,EBreite,Dist,x,y,z)
  END;

  PROCEDURE ELBtoVec(ELaenge,EBreite,Dist:Skalar;VAR x,y,z:Skalar);
  BEGIN
    ELaenge:=DegtoRad(ELaenge); EBreite:=DegtoRad(EBreite);
    x:=Dist*Cos(EBreite)*Cos(ELaenge);
    y:=Dist*Cos(EBreite)*Sin(ELaenge);
    z:=Dist*Sin(EBreite)
  END;

  PROCEDURE VectoELB(x,y,z:Skalar;VAR ELaenge,EBreite,Dist:Skalar);
  BEGIN
    Dist:=Sqrt(x*x+y*y+z*z);
    IF y=0 THEN IF x<0 THEN ELaenge:=180 ELSE ELaenge:=0
           ELSE IF x=0 THEN IF y<0 THEN ELaenge:=270 ELSE ELaenge:=90
                       ELSE
                       BEGIN
                         ELaenge:=RadtoDeg(ArcSin(y/Sqrt(x*x+y*y)));
                         IF (x<0) THEN ELaenge:=180-ELaenge
                           ELSE IF y<0 THEN ELaenge:=360+ELaenge
                       END;
    IF (z=0) OR (Dist=0) THEN EBreite:=0 ELSE EBreite:=RadtoDeg(ArcSin(z/Dist))
  END;

  PROCEDURE RungeKutta(VAR Data:TRKInfo);
  VAR kp,kv:ARRAY[1..MaxKoerper,1..4] OF TVector;
      ap,av:ARRAY[1..MaxKoerper] OF TVector;
      Abstand,dx,dy,dz:Skalar;
      Zeit:Skalar;
      Lauf:Byte;
      IAnz,MaxSchritte:LongInt;
    PROCEDURE Diffloesen(num:Byte);
    VAR i,k:Byte;
    BEGIN
      WITH Data DO
      BEGIN
        FOR i:=1 TO AnzK DO WITH Koerper[i] DO
          IF Status THEN
          BEGIN
            kp[i,num]:=Velo;
            kv[i,num]:=Nullvector;
            FOR k:=1 TO AnzK DO IF k<>i THEN
              IF Koerper[k].Status THEN
              BEGIN
                dx:=Posi.x-Koerper[k].Posi.x;
                dy:=Posi.y-Koerper[k].Posi.y;
                dz:=Posi.z-Koerper[k].Posi.z;
                Abstand:=Sqrt(dx*dx+dy*dy+dz*dz);
                Abstand:=Abstand*Abstand*Abstand;
                kv[i,num].x:=kv[i,num].x-f*Koerper[k].Masse*dx/Abstand;
                kv[i,num].y:=kv[i,num].y-f*Koerper[k].Masse*dy/Abstand;
                kv[i,num].z:=kv[i,num].z-f*Koerper[k].Masse*dz/Abstand
              END
          END
      END
    END;
    PROCEDURE v_p_Veraendern(Schritt:Skalar;num:Byte);
    VAR i:Byte;
    BEGIN
      WITH Data DO FOR i:=1 TO AnzK DO WITH Koerper[i] DO
        IF Status THEN
        BEGIN
          WITH Posi DO
          BEGIN
            x:=ap[i].x+Schritt*kp[i,num].x;
            y:=ap[i].y+Schritt*kp[i,num].y;
            z:=ap[i].z+Schritt*kp[i,num].z
          END;
          WITH Velo DO
          BEGIN
            x:=av[i].x+Schritt*kv[i,num].x;
            y:=av[i].y+Schritt*kv[i,num].y;
            z:=av[i].z+Schritt*kv[i,num].z
          END
        END
    END;
  BEGIN
    WITH Data DO
    BEGIN
      Zeit:=0; IAnz:=0; Tage:=Tage*SekproTag;
      MaxSchritte:=Trunc(Tage/Faktor);
      WHILE IAnz<MaxSchritte DO
      BEGIN
        FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
        BEGIN
          ap[Lauf]:=Posi; av[Lauf]:=Velo
        END;
                (* Runge-Kutta Verfahren anwenden *)
        Diffloesen(1); v_p_Veraendern(Faktor/2,1);
        Diffloesen(2); v_p_Veraendern(Faktor/2,2);
        Diffloesen(3); v_p_Veraendern(Faktor,3);
        Diffloesen(4);
        FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
          IF Status THEN
          BEGIN
                  (* 1. Differentialgleichung lîsen *)
            WITH Posi DO
            BEGIN
              x:=ap[Lauf].x+Faktor/6*(kp[Lauf,1].x+2*kp[Lauf,2].x+2*kp[Lauf,3].x+kp[Lauf,4].x);
              y:=ap[Lauf].y+Faktor/6*(kp[Lauf,1].y+2*kp[Lauf,2].y+2*kp[Lauf,3].y+kp[Lauf,4].y);
              z:=ap[Lauf].z+Faktor/6*(kp[Lauf,1].z+2*kp[Lauf,2].z+2*kp[Lauf,3].z+kp[Lauf,4].z)
            END;
                  (* 2. Differentialgleichung lîsen *)
            WITH Velo DO
            BEGIN
              x:=av[Lauf].x+Faktor/6*(kv[Lauf,1].x+2*kv[Lauf,2].x+2*kv[Lauf,3].x+kv[Lauf,4].x);
              y:=av[Lauf].y+Faktor/6*(kv[Lauf,1].y+2*kv[Lauf,2].y+2*kv[Lauf,3].y+kv[Lauf,4].y);
              z:=av[Lauf].z+Faktor/6*(kv[Lauf,1].z+2*kv[Lauf,2].z+2*kv[Lauf,3].z+kv[Lauf,4].z)
            END;
          END
          ELSE
          BEGIN
            Posi:=ap[Lauf]; Velo:=av[Lauf]
          END;
        Zeit:=Zeit+Faktor; Inc(IAnz);
        IF IAnz MOD I2=0 THEN RegCall(AnzK,Koerper,IAnz,Infos);
      END;
      Tage:=Tage-Zeit
    END
  END;

  PROCEDURE DiffMatrix(VAR Data:TRKInfo;Matrix:TMatrix;MDim:Byte);
  VAR kp,kv:ARRAY[1..MaxKoerper,1..8] OF TVector;
      ap,av:ARRAY[1..MaxKoerper] OF TVector;
      GravMass:ARRAY[1..MaxKoerper] OF Skalar;
      delta:TVector;
      Abstand,Zeit,LightWay:Skalar;
      Lauf,Lauf2:Byte;
      IAnz,MaxSchritte:LongInt;
    PROCEDURE DiffLoesen(num,bodynr:Byte;VAR Koerper:TSSystem);
    VAR k:Byte;
    BEGIN
      WITH Data DO WITH Koerper[bodynr] DO
        IF Status THEN
        BEGIN
          kp[bodynr,num]:=Velo;
          kv[bodynr,num]:=NullVector;
          FOR k:=1 TO AnzK DO IF k<>bodynr THEN
            IF Koerper[k].Status THEN
            BEGIN
              VSub(delta,Posi,Koerper[k].Posi);
              Abstand:=VLen(delta);
              Abstand:=Abstand*Abstand*Abstand;
              kv[bodynr,num].x:=kv[bodynr,num].x-GravMass[k]*delta.x/Abstand;
              kv[bodynr,num].y:=kv[bodynr,num].y-GravMass[k]*delta.y/Abstand;
              kv[bodynr,num].z:=kv[bodynr,num].z-GravMass[k]*delta.z/Abstand
            END
        END
    END;
    PROCEDURE v_p_Veraendern(num,bodynr:Byte;Faktor:Skalar;VAR Koerper:TSSystem);
    VAR j:Byte;
    BEGIN
      WITH Data DO WITH Koerper[bodynr] DO
        IF Status THEN
        BEGIN
          WITH Posi DO
          BEGIN
            Posi:=ap[bodynr];
            FOR j:=1 TO num DO
            BEGIN
              x:=x+Faktor*Matrix[num+1,j+1]*kp[bodynr,j].x;
              y:=y+Faktor*Matrix[num+1,j+1]*kp[bodynr,j].y;
              z:=z+Faktor*Matrix[num+1,j+1]*kp[bodynr,j].z
            END
          END;
          WITH Velo DO
          BEGIN
            Velo:=av[bodynr];
            FOR j:=1 TO num DO
            BEGIN
              x:=x+Faktor*Matrix[num+1,j+1]*kv[bodynr,j].x;
              y:=y+Faktor*Matrix[num+1,j+1]*kv[bodynr,j].y;
              z:=z+Faktor*Matrix[num+1,j+1]*kv[bodynr,j].z
            END
          END
        END
    END;
    PROCEDURE One_step;
    VAR Lauf,Lauf2:Byte;
    BEGIN
      WITH Data DO
      BEGIN
        FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
        BEGIN
          ap[Lauf]:=Posi; av[Lauf]:=Velo
        END;
                (* Verfahren anwenden *)
        FOR Lauf:=1 TO MDim-1 DO
        BEGIN
          FOR Lauf2:=1 TO AnzK DO
            Diffloesen(Lauf,Lauf2,Koerper);
          FOR Lauf2:=1 TO AnzK DO
            v_p_Veraendern(Lauf,Lauf2,Faktor,Koerper)
        END;
                (* Beim letzten Schritt nur den Korr.-faktor bestimmen *)
        FOR Lauf2:=1 TO AnzK DO Diffloesen(MDim,Lauf2,Koerper);
        FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
          IF Status THEN
          BEGIN
                (* 1. Differentialgleichung lîsen *)
            WITH Posi DO
            BEGIN
              delta:=NullVector;
              FOR Lauf2:=1 TO MDim DO
              BEGIN
                delta.x:=delta.x+Faktor*Matrix[MDim+1,Lauf2]*kp[Lauf,Lauf2].x;
                delta.y:=delta.y+Faktor*Matrix[MDim+1,Lauf2]*kp[Lauf,Lauf2].y;
                delta.z:=delta.z+Faktor*Matrix[MDim+1,Lauf2]*kp[Lauf,Lauf2].z
              END;
              VAdd(Posi,ap[Lauf],delta)
            END;
                (* 2. Differentialgleichung lîsen *)
            WITH Velo DO
            BEGIN
              delta:=NullVector;
              FOR Lauf2:=1 TO MDim DO
              BEGIN
                delta.x:=delta.x+Faktor*Matrix[MDim+1,Lauf2]*kv[Lauf,Lauf2].x;
                delta.y:=delta.y+Faktor*Matrix[MDim+1,Lauf2]*kv[Lauf,Lauf2].y;
                delta.z:=delta.z+Faktor*Matrix[MDim+1,Lauf2]*kv[Lauf,Lauf2].z
              END;
              VAdd(Velo,av[Lauf],delta)
            END
          END
          ELSE
          BEGIN
            Posi:=ap[Lauf]; Velo:=av[Lauf]
          END
      END
    END;
  BEGIN
    WITH Data DO
    BEGIN
        (* Simulationsdaten zurÅcksetzen *)
      Zeit:=0; IAnz:=0; Tage:=Tage*SekproTag;
      MaxSchritte:=Trunc(Tage/Faktor);

        (* GravMass=Gravitationskonstante*Kîrpermasse (optimiert Zeit!) *)
      FOR Lauf:=1 TO AnzK DO
        GravMass[Lauf]:=f*Koerper[Lauf].Masse;

        (* Die Matrix mit der Iterationsschrittweite multiplizieren *)
      {FOR Lauf:=1 TO MDim DO
        FOR Lauf2:=1 TO MDim+1 DO
          Matrix[Lauf2,Lauf]:=Matrix[Lauf2,Lauf]*Faktor;}

        (* Die Iteration durchfÅhren; regelmÑ·ig "RegCall" aufrufen *)
      WHILE IAnz<MaxSchritte DO
      BEGIN
        One_step;
        Zeit:=Zeit+Faktor; Inc(IAnz);
        IF IAnz MOD I2=0 THEN RegCall(AnzK,Koerper,IAnz,Infos)
      END;

        (* TatsÑchlich iterierte Zeit ermitteln; Restzeit ist "Tage" *)
      Tage:=Tage-Zeit;

    END
  END;

  PROCEDURE Dummy(VAR AnzK:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR Infos:Pointer);
  BEGIN
  END;

  PROCEDURE GetSpeed(VAR Data:TGSInfo);
  CONST MaxOptiS=30;    { Maximale Anzahl Fehler, die zurÅckverfolgt werden }
  TYPE FFeld=ARRAY[1..MaxOptiS] OF Skalar;
  VAR AKoerper:PSSystem;
      Punkt3:^TVecArray;
      Lauf,Lauf2:Byte;
      AKorr:ARRAY [1..MaxKoerper] OF TVector; { Alte Korrekturen von vx,vy und vz }
      Daten:^TRKInfo;   { Informationen fÅr das Runge-Kutta Verfahren }
      LastValue:^FFeld; { Letzte Abbruchfehler }
      LVCount:Byte;     { ZÑhlvariable fÅr LastValue }
    PROCEDURE InitItera;
    VAR Lauf:Byte;
    BEGIN
      New(Daten); New(AKoerper); New(Punkt3); New(LastValue);
      WITH Data DO
      BEGIN
        LVCount:=1;
        FOR Lauf:=1 TO AnzK DO
          WITH Koerper[Lauf] DO
          BEGIN
            IF UpDate=Aktiv THEN WITH Velo DO
            BEGIN
              x:=(Ziel[Lauf].X-Start[Lauf].X)/Weite/Faktor;
              y:=(Ziel[Lauf].Y-Start[Lauf].Y)/Weite/Faktor;
              z:=(Ziel[Lauf].Z-Start[Lauf].Z)/Weite/Faktor
            END;
            Posi:=Start[Lauf];
            AKorr[Lauf]:=Nullvector
          END;
        Daten^.AnzK:=AnzK;
        Daten^.Koerper:=Koerper;
        Daten^.Faktor:=Faktor;
        Daten^.Tage:=Weite*Faktor/SekproTag;
        Daten^.I2:=Weite;
        Daten^.RegCall:=Dummy
      END
    END;
    PROCEDURE DeInitItera;
    VAR Lauf:Byte;
    BEGIN
      Dispose(Daten); Dispose(AKoerper); Dispose(Punkt3); Dispose(LastValue)
    END;
    PROCEDURE Determination;
    VAR Hilf:TVector;
        Lauf:Byte;
    BEGIN
      WITH Data DO
      BEGIN
        AktFehler:=0;
        AKoerper^:=Koerper;
        Daten^.Koerper:=Koerper; Daten^.Tage:=Weite*Faktor/SekproTag;
        RungeKutta(Daten^);
        Koerper:=Daten^.Koerper;
        FOR Lauf:=1 TO AnzK DO IF Koerper[Lauf].UpDate=Aktiv THEN
        BEGIN
          Punkt3^[Lauf]:=Koerper[Lauf].Posi;
          VSub(Hilf,Ziel[Lauf],Punkt3^[Lauf]);
          IF VLen(Ziel[Lauf])<>0 THEN AktFehler:=AktFehler+VLen(Hilf)/VLen(Ziel[Lauf])
        END;
        Koerper:=AKoerper^
      END
    END;
    PROCEDURE VarSpeed;
    VAR Lauf:Byte;
        Korr:ARRAY[1..MaxKoerper] OF TVector;
    BEGIN
      WITH Data DO
      BEGIN
        FOR Lauf:=1 TO AnzK DO
          WITH Koerper[Lauf] DO IF UpDate=Aktiv THEN
          BEGIN
            Korr[Lauf].x:=Alpha*(Ziel[Lauf].X-Punkt3^[Lauf].X)/Weite/Faktor;
            Korr[Lauf].y:=Alpha*(Ziel[Lauf].Y-Punkt3^[Lauf].Y)/Weite/Faktor;
            Korr[Lauf].z:=Alpha*(Ziel[Lauf].Z-Punkt3^[Lauf].Z)/Weite/Faktor;
            WITH Velo DO
            BEGIN
              x:=x+Korr[Lauf].x+Eta*AKorr[Lauf].x;
              y:=y+Korr[Lauf].y+Eta*AKorr[Lauf].y;
              z:=z+Korr[Lauf].z+Eta*AKorr[Lauf].z
            END;
            AKorr[Lauf]:=Korr[Lauf]
          END
        END
    END;
    FUNCTION Schon_gehabt(F:Skalar):Boolean;
    VAR Lauf:Byte;
    BEGIN
      Schon_gehabt:=FALSE;
      FOR Lauf:=1 TO LVCount DO Schon_gehabt:=(F=LastValue^[Lauf])
    END;
  BEGIN
    InitItera; Data.AktFehler:=1E10;
    REPEAT
      IF LVCount<=MaxOptiS THEN
      BEGIN LastValue^[LVCount]:=Data.AktFehler; Inc(LVCount) END
      ELSE
      BEGIN
        FOR Lauf2:=1 TO MaxOptiS-1 DO LastValue^[Lauf2]:=LastValue^[Lauf2]+1;
        Dec(LVCount)
      END;
      Determination;
      VarSpeed;
      WriteLn('Aktueller Fehler:',Data.AktFehler)
    UNTIL Schon_gehabt(Data.AktFehler) OR KeyPressed;
    DeInitItera
  END;

  FUNCTION EKin(Nummer:Byte;VAR Koerper:TSSystem):Skalar;
  VAR Lauf:Byte;
      GVec:TVector;
  BEGIN
    WITH Koerper[Nummer] DO
      EKin:=0.5*Masse*VLen(Velo)*VLen(Velo)
  END;

  FUNCTION EPot(Nummer,AnzK:Byte;VAR Koerper:TSSystem):Skalar;
  VAR Lauf:Byte;
      Hilf,Dist:Skalar;
  BEGIN
    Hilf:=0;
    FOR Lauf:=1 TO AnzK DO
    BEGIN
      IF Lauf<>Nummer THEN
      BEGIN
        Dist:=Sqrt(Sqr(Koerper[Lauf].Posi.x-Koerper[Nummer].Posi.x)+
                   Sqr(Koerper[Lauf].Posi.y-Koerper[Nummer].Posi.y)+
                   Sqr(Koerper[Lauf].Posi.z-Koerper[Nummer].Posi.z));
        Hilf:=Hilf-f*Koerper[Lauf].Masse*Koerper[Nummer].Masse/Dist
      END
    END;
    EPot:=-Hilf
  END;

  FUNCTION Energie(AnzK:Byte;VAR Koerper:TSSystem):Skalar;
  VAR Lauf,Lauf2:Byte;
      Hilf,Dist:Skalar;
  BEGIN
    Hilf:=0;
    FOR Lauf:=1 TO AnzK DO
    BEGIN
      Hilf:=Hilf+EKin(Lauf,Koerper);
      FOR Lauf2:=Lauf TO AnzK DO IF Lauf2<>Lauf THEN WITH Koerper[Lauf] DO
      BEGIN
        Dist:=Sqrt(Sqr(Posi.x-Koerper[Lauf2].Posi.x)+
                   Sqr(Posi.y-Koerper[Lauf2].Posi.y)+
                   Sqr(Posi.z-Koerper[Lauf2].Posi.z));
        Hilf:=Hilf-f*Masse*Koerper[Lauf2].Masse/Dist
      END
    END;
    Energie:=Hilf
  END;

  PROCEDURE Schwerpunkt(AnzK:Byte;VAR Koerper:TSSystem;VAR SPunkt:TVector);
  VAR Lauf:Byte;
      Massen:Skalar;
  BEGIN
    Massen:=0;
    FOR Lauf:=1 TO AnzK DO Massen:=Massen+Koerper[Lauf].Masse;
    WITH SPunkt DO
    BEGIN
      x:=0; y:=0; z:=0;
      FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
      BEGIN
        x:=x+Posi.x*Masse/Massen;
        y:=y+Posi.y*Masse/Massen;
        z:=z+Posi.z*Masse/Massen
      END
    END
  END;

  PROCEDURE Eclipse(AnzK:Byte;VAR Koerper:TSSystem;VAR FAbst,halb,voll:Skalar;
                    VAR FTyp:TDarkness);
  VAR Mond,Erde,Erde_,Mond_,Sonne,RadVec,PHeavyPoint,PBaseLen,
        RayBase1,RayBase2,PFullS,PHalfS,Hilf:TVector;
      ProjPlane:TPlane;
  BEGIN
    Mond:=Koerper[MondNr].Posi;
    Erde:=Koerper[ErdNr].Posi;
    Sonne:=Koerper[SonneNr].Posi;
    VSub(Erde_,Erde,Sonne);
    VSub(Mond_,Mond,Sonne);
      (* Art der Finsternis *)
    IF VLen(Mond_)>VLen(Erde_) THEN FTyp:=MondF ELSE FTyp:=SonF;
    IF FTyp=MondF THEN
      (* FÅr Mondfinsternisse *)
    BEGIN
      WITH ProjPlane DO WITH Mond_ DO
      BEGIN A:=X; B:=Y; C:=Z; D:=-A*Mond.X-B*Mond.Y-C*Mond.Z END;
        (* Projektion des Erdschwerpunktes auf die Sonne/Mond-Ebene *)
      LP_IntSec(ProjPlane,Sonne,Erde,PHeavyPoint);
      VSub(PBaseLen,PHeavyPoint,Mond);
      FAbst:=VLen(PBaseLen);
        (* Projektion der Randstrahlen auf die Sonne/Mond-Ebene *)
      WITH RadVec DO
      BEGIN
        x:=0; y:=Erde_.z; z:=-Erde_.y
      END;
        (* Obere Sonnenseite/obere Erdseite *)
      VecNorm(RadVec); VSMult(RadVec,PRadius[SonneNr]);
      VAdd(RayBase1,Sonne,RadVec);
      VecNorm(RadVec); VSMult(RadVec,ErdRadius);
      VAdd(RayBase2,Erde,RadVec);
      LP_IntSec(ProjPlane,RayBase1,RayBase2,PFullS);
      VSub(PFullS,PHeavyPoint,PFullS);
      voll:=VLen(PFullS);
        (* Untere Sonnenseite/obere Erdseite *)
      VecNorm(RadVec); VSMult(RadVec,PRadius[SonneNr]);
      VSub(RayBase1,Sonne,RadVec);
      LP_IntSec(ProjPlane,RayBase1,RayBase2,PHalfS);
      VSub(PHalfS,PHeavyPoint,PHalfS);
      halb:=VLen(PHalfS)
    END
    ELSE
      (* FÅr Sonnenfinsternisse *)
    BEGIN
      WITH ProjPlane DO WITH Erde_ DO
      BEGIN A:=X; B:=Y; C:=Z; D:=-A*Erde.X-B*Erde.Y-C*Erde.Z END;
        (* Projektion des Mondschwerpunktes auf die Sonne/Erde-Ebene *)
      LP_IntSec(ProjPlane,Sonne,Mond,PHeavyPoint);
      VSub(PBaseLen,PHeavyPoint,Erde);
      FAbst:=VLen(PBaseLen);
        (* Projektion der Randstrahlen auf die Sonne/Erde-Ebene *)
      WITH RadVec DO
      BEGIN
        x:=0; y:=Mond_.z; z:=-Mond_.y
      END;
        (* Obere Sonnenseite/obere Mondseite *)
      VecNorm(RadVec); VSMult(RadVec,PRadius[SonneNr]);
      VAdd(RayBase1,Sonne,RadVec);
      VecNorm(RadVec); VSMult(RadVec,MondRadius);
      VAdd(RayBase2,Mond,RadVec);
      LP_IntSec(ProjPlane,RayBase1,RayBase2,PFullS);
      VSub(PFullS,PHeavyPoint,PFullS);
      voll:=VLen(PFullS);
        (* Untere Sonnenseite/obere Erdseite *)
      VecNorm(RadVec); VSMult(RadVec,PRadius[SonneNr]);
      VSub(RayBase1,Sonne,RadVec);
      LP_IntSec(ProjPlane,RayBase1,RayBase2,PHalfS);
      VSub(PHalfS,PHeavyPoint,PHalfS);
      halb:=VLen(PHalfS)
    END;
    IF KeyPressed THEN
    BEGIN
      WriteLn;
      WriteLn('Prinzipiell: ',Ord(FTyp));
      WriteLn('Distanz: ',FAbst:1:10,'(=',FAbst/ErdRadius:1:10,'ER)');
      WriteLn('Halbschatten: ',halb:1:10);
      WriteLn('Totalschatten: ',voll:1:10);
      VSub(Hilf,Koerper[ErdNr].Posi,Koerper[MondNr].Posi);
      WriteLn('Distanz der Kîrper: ',VLen(Hilf)/ErdRadius:1:10,'ER')
    END;
    IF ((FTyp=MondF) AND (FAbst>Halb+MondRadius)) OR
       ((FTyp=SonF) AND (FAbst>Halb+ErdRadius)) THEN FTyp:=Keine
       ELSE
       BEGIN
         {WriteLn;
         WriteLn('Hinreichendes Kriterium fÅr Finsternis gefunden !')}
       END
  END;


BEGIN
  SonneNr:=1; ErdNr:=2; MondNr:=3; MarsNr:=6; SaturnNr:=8; PlutoNr:=11
END.
