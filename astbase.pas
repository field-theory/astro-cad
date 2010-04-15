PROGRAM AstroCAD(Input,Output);


{
        Astronomisches CAD-Programm zur Bestimmung von Planetenbahnen mittels
        der Lîsung von jeweils 6 Differentialgleichungen fÅr einen Kîrper;
        autom. Verwaltung von Positionsdateien und graphischer Analyse und
        abschlie·ender Koordinatenumrechnung fÅr alle Astronomen, die Åber die
        'TrockenÅbungen' hinaus kommen wollen.

                                      ASTRO-CAD V0.1a
                           (c) 1990,91,92 by Wolfram Schroers
}


USES Vectors,AstConv,AstVerw;


{
        *** 1. Teil: Globale Variablen und Konstanten ***
}


VAR     JulianDate      : Skalar;       { Julianisches Datum }
        UT,MOZ          : Skalar;       { Verschiedene Zeiten }
        SonnenSystem    : TRKInfo;      { Daten fÅr Runge-Kutta-Verfahen }


{
        *** 2. Teil: Unterprogramme und Initialisierungen ***
}


  PROCEDURE Init;
  VAR Lauf:Byte;
  BEGIN
        (* Variablen initialisieren *)
    SonnenSystem.AnzK:=0;
    SonnenSystem.Faktor:=600; { Schrittweite Runge-Kutta-Verfahren }
    JulianDate:=2448256.5; { Dummy }
    UT:=0;
    FOR Lauf:=1 TO MaxKoerper DO New(SonnenSystem.Koerper[Lauf].Info)
  END;

  PROCEDURE DeInit;
  VAR Lauf:Byte;
  BEGIN
    FOR Lauf:=1 TO MaxKoerper DO Dispose(SonnenSystem.Koerper[Lauf].Info);
    WriteLn('Vielen Dank fÅr die Arbeit mit AstroCAD !')
  END;

  PROCEDURE ZeigePosition(VAR A:Byte;VAR Koerper:TSSystem;VAR I:LongInt); Far;
  BEGIN
    JulianDate:=JulianDate+1
  END;

  PROCEDURE Ausfuehren;
  VAR Daten:DiskSystem;
      Lauf:Byte;
      TeXDatei:Text;
      SPhenDaten:SPhen;
      Anzeige:TWatch;
      aktpage:Byte;
      last:Boolean;
      Wahl:Char;
      Ende:Boolean;
    PROCEDURE Laden(DName:String);
    BEGIN
      WITH SonnenSystem DO
      BEGIN
        Daten.AnzK:=AnzK;
        Daten.Koerper:=Addr(Koerper);
        Daten.JulDat:=JulianDate;
        Daten.Dateiname:=DName
      END;
      LoadBodies(Daten);
      SonnenSystem.AnzK:=Daten.AnzK;
      JulianDate:=Daten.JulDat;
      IF Daten.OK THEN
        WriteLn(SonnenSystem.AnzK:2,' Koerper eingelesen.')
      ELSE
        WriteLn('Fehler beim Einlesen. Bitte brechen Sie ab !');
    END;
    PROCEDURE Speichern;
    BEGIN
      aktpage:=0;
      Daten.Dateiname:='SOLARUPD.TSS';
      Daten.Beschreib:='Unser Sonnensystem am Ende eines Analyselaufes';
      WITH Daten DO
      BEGIN
        AnzK:=SonnenSystem.AnzK;
        Koerper:=Addr(SonnenSystem.Koerper);
        JulDat:=JulianDate
      END;
      SaveBodies(Daten)
    END;
    PROCEDURE Ereignisse_suchen;
    BEGIN
      WITH SPhenDaten DO
      BEGIN
        AnzK:=SonnenSystem.AnzK;
        Von:=ErdNr; Bis:=SonnenSystem.AnzK;
        Koerper:=Addr(SonnenSystem.Koerper);
        Faktor:=SonnenSystem.Faktor;
        JulDat:=JulianDate;
        Tage:=600;
        Assign(ToDat,'jup2.dat');
        ReWrite(ToDat);
        Bahn:=NichtAktiv;
        Konstell:=Aktiv;
        Eclipse:=Aktiv
      END;
      SolarPheno(SPhenDaten);
    END;
  BEGIN
    Laden('SOLARST.TSS');
    Ereignisse_suchen;
    Speichern
  END;

BEGIN
  Init;
  Ausfuehren;
  DeInit
END.
