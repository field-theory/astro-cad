PROGRAM AstroCADEnter(Input,Output);

{
  Zweite Variante des AstroCAD-Eingabeprogrammes, die die Eingabe des
  Sonnensystems speziell nach den Daten im "Astronomical Almanac" unterstÅtzt.
  Dabei wird der Schwerpunktsatz zugrundegelegt und intern verschiedene
  Koordinatensysteme gewÑhlt.
}

{$N+,E+,S+,R-}
{$M 32767,2048,163840}

USES AstConv,AstVerw,Vectors;

CONST BodyName:String[12]='SOLARST.TSS';
      PosName:String[12]='SOLARST.POS';
      Ersteingabe=TRUE;
      Zweiteingabe=FALSE;

VAR Bodies:Text;
    JulDat:Skalar;
    GSDaten:TGSInfo;
    AnzTage:LongInt;

  PROCEDURE Init;
  BEGIN
    WriteLn('AstroCAD-II v0.1a - Testversion der erweiterten Eingabe');
    WriteLn('UnterstÅtzt speziell die Dateneingabe aus dem');
    WriteLn('"Astronomical Almanac" unter Verwendung des Schwerpunktsatzes');
    WriteLn('(c) 1990,91,92 by Wolfram Schroers');
    WriteLn
  END;

  PROCEDURE CreatePlanetFile;
  VAR h,m,s:Skalar;
      rek,dekl,elaenge,ebreite,rvektor,masse:Skalar;
      Lauf:Byte;
      Beschreib,ID:String;
      MMittel:TVector;
    PROCEDURE GetPos(VAR x,y,z:Skalar;VAR Descrip:Integer;Erst:Boolean);
    VAR Wahl:Char;
	Ekliptik,ErdRad,Parallaxe:Boolean;
	VAR Zeile:String;
    BEGIN
      WITH GSDaten DO
      BEGIN
	IF Lauf=ErdNr THEN
	BEGIN
	  WriteLn('Die Raumkoordinaten der Erde werden automatisch auf 0 gesetzt !');
	  x:=0; y:=0; z:=0; Descrip:=8
	END
	ELSE
	BEGIN
	  IF Erst THEN
	  BEGIN
	    Descrip:=0;
	    Write('Daten in HimmelsÑquator- oder Ekliptikalen Koordinaten (H/E) ?');
	    ReadLn(Wahl); Wahl:=UpCase(Wahl); WriteLn(Wahl); IF Wahl='E' THEN Descrip:=Descrip+1
	  END;
	  Ekliptik:=Descrip MOD 2<>0;
	  Write('Bitte geben Sie die ');
	  IF NOT Ekliptik THEN Write('Rektaszension an (Format: h m s):')
	    ELSE Write('ekliptikale LÑnge an (Format: g m s):');
	  ReadLn(Zeile); StrtoDeg(Zeile,h,m,s);
	  WriteLn('Eingegeben:',h:5:2,'  ',m:5:2,'  ',s:8:3);
	  IF NOT Ekliptik THEN
	    IF Lauf=MondNr THEN rek:=DegtoDeg(h,m,s) ELSE rek:=RAtoDeg(h,m,s)
	    ELSE elaenge:=DegtoDeg(h,m,s);
	  Write('Bitte geben Sie die ');
	  IF NOT Ekliptik THEN Write('Deklination') ELSE Write('ekliptikale Breite');
	  Write(' an (Format wie oben):');
	  ReadLn(Zeile); StrtoDeg(Zeile,h,m,s);
	  WriteLn('Eingegeben:',h:5:2,'  ',m:5:2,'  ',s:8:3);
	  IF NOT Ekliptik THEN dekl:=DegtoDeg(h,m,s) ELSE ebreite:=DegtoDeg(h,m,s);
	  IF NOT Ekliptik THEN RaDtoELB(rek,dekl,elaenge,ebreite,JulDat);
	  IF Erst THEN
	  BEGIN
	    Write('Einheit des Radiusvektors (Erdradien, Astronom. Einheiten) (E/A) ?');
	    ReadLn(Wahl); Wahl:=UpCase(Wahl); WriteLn(Wahl); IF Wahl='E' THEN
	    BEGIN
	      Descrip:=Descrip+2;
	      Write('Normaler Erdradius oder Parallaxe (E/P) ?');
	      ReadLn(Wahl); Wahl:=UpCase(Wahl); WriteLn(Wahl);
	      IF Wahl='P' THEN Descrip:=Descrip+16
	    END
	  END;
	  ErdRad:=((Descrip SHR 1) AND 1)=1; Parallaxe:=((Descrip SHR 4) AND 1)=1;
	  Write('Radiusvektor: '); ReadLn(rvektor);
	  IF ErdRad THEN
	    IF Parallaxe THEN rvektor:=ErdRadius/Sin(DegtoRad(rvektor))
	    ELSE rvektor:=rvektor*ErdRadius ELSE rvektor:=rvektor*AstroUnit;
	   (* Sonderbehandlung zur Eingabe von parallaktischen Koordinaten ! *)
	  ELBtoVec(elaenge,ebreite,rvektor,x,y,z);
	  IF Erst THEN
	    BEGIN
	      Write('Geozentrische Koordinaten (J/N) ?');
	      ReadLn(Wahl); Wahl:=UpCase(Wahl); WriteLn(Wahl);
	      IF Wahl='J' THEN Descrip:=Descrip+4
	    END;
	    IF ((Descrip SHR 2) AND 1)<>1 THEN
	      IF Erst THEN
	      BEGIN
		x:=x+Start[SonneNr].x; y:=y+Start[SonneNr].y; z:=z+Start[SonneNr].z
	      END
	      ELSE
	      BEGIN
		x:=x+Ziel[SonneNr].x; y:=y+Ziel[SonneNr].y; z:=z+Ziel[SonneNr].z
	      END
	END
      END
    END;
    PROCEDURE ReadBody(VAR Koerper:THKoerper;Lauf:Byte);
    VAR Wahl:Char;
    BEGIN
      WriteLn('Kîrper',Lauf:2,':');
      WITH Koerper DO
      BEGIN
	New(Koerper.Info);
	Status:=Aktiv;
	WriteLn('Bitte geben Sie die Position des Kîrpers im Anfangsstadium ein:');
	WITH GSDaten.Start[Lauf] DO
	BEGIN GetPos(X,Y,Z,Info^.Descrip,Ersteingabe); Posi.X:=X; Posi.Y:=Y; Posi.Z:=Z END;
	IF Lauf IN [1..8] THEN
	BEGIN
	  WriteLn('Masse wird nach obiger Vorgabe automatisch determiniert !');
	  IF Lauf=MondNr THEN Masse:=GSDaten.Koerper[ErdNr].Masse*MondErdeMassenVerhaeltnis
	  ELSE
	    Masse:=SonnenMasse/PlanetenMasse[Lauf]
	END
	ELSE
	BEGIN
	  WriteLn('Wird die Masse des Kîrpers in kg, im VerhÑltnis zur Sonne oder im VerhÑltnis');
	  Write('zur Erde (als Kehrwert) angegeben (K/S/E) ?');
	  ReadLn(Wahl); Wahl:=UpCase(Wahl); WriteLn(Wahl);
	  Write('Masse des Kîrpers: '); ReadLn(Masse);
	  IF Wahl='S' THEN Masse:=SonnenMasse/Masse
	    ELSE IF Wahl='E' THEN Masse:=GSDaten.Koerper[ErdNr].Masse*Masse
	END;
	UpDate:=Aktiv; Status:=Aktiv;
	WriteLn('Bitte geben Sie die Position nach m Iterationsschritten an:');
	WITH GSDaten.Ziel[Lauf] DO GetPos(X,Y,Z,Info^.Descrip,Zweiteingabe);
	WriteLn('Bitte geben Sie eine kurze Beschreibung des Kîrpers ein:');
	ReadLn(Info^.Beschreib)
      END
    END;
  BEGIN
    Write('Anzahl der Kîrper:'); ReadLn(GSDaten.AnzK);
    Write('Julianisches Datum bei Iterationsstart:'); ReadLn(JulDat);
    Assign(Bodies,BodyName);
    WriteLn('Kurzbeschreibung dieser TSS-Datei zu Katalogisierungszwecken:');
    ReadLn(Beschreib);
    Write('Anzahl der Iterationschritte, die zwischen den beiden Punkten liegen:');
    ReadLn(GSDaten.Weite);
    Write('Faktor Alpha (empfohlen: 1.0):'); ReadLn(GSDaten.Alpha);
    Write('Faktor Eta (empfohlen: 0.0):'); ReadLn(GSDaten.Eta);
    WriteLn;
    WriteLn('Bitte geben Sie nun die einzelnen Kîrper der Reihe nach an.');
    WriteLn('Es wird ein entspr. Datenfile auf Diskette erzeugt, nachdem');
    WriteLn('die Daten vervollstÑndigt wurden!');
    WriteLn;
    WriteLn('Automatisch werden folgende Informationen angenommen:');
    WriteLn('1 - Sonne');
    WriteLn('2 - Erde');
    WriteLn('3 - Mond');
    WriteLn('4 - Merkur');
    WriteLn('5 - Venus');
    WriteLn('6 - Mars');
    WriteLn('usw...');
    FOR Lauf:=1 TO GSDaten.AnzK DO
    BEGIN
      ReadBody(GSDaten.Koerper[Lauf],Lauf);
      WITH GSDaten.Koerper[Lauf].Info^ DO
      BEGIN
	IF Lauf=SonneNr THEN
	BEGIN KoerperTyp:=Sonne; ZentKoerp:=Lauf END
	ELSE
	  IF Lauf=MondNr THEN
	  BEGIN KoerperTyp:=Mond; ZentKoerp:=ErdNr END
	  ELSE
	    BEGIN
	      KoerperTyp:=Planet; ZentKoerp:=SonneNr
	    END
      END
    END;
    WITH GSDaten DO
    BEGIN
      Schwerpunkt(AnzK,Koerper,MMittel);
      FOR Lauf:=1 TO AnzK DO VSub(Start[Lauf],Start[Lauf],MMittel);
      FOR Lauf:=1 TO AnzK DO Koerper[Lauf].Posi:=Ziel[Lauf];
      Schwerpunkt(AnzK,Koerper,MMittel);
      FOR Lauf:=1 TO AnzK DO VSub(Ziel[Lauf],Ziel[Lauf],MMittel);
      FOR Lauf:=1 TO AnzK DO Koerper[Lauf].Posi:=Start[Lauf]
    END;
    WriteLn;
    WriteLn('Datenerfassung beendet! Ermittlung der v-Vektoren ...');
    GetSpeed(GSDaten);
    WriteLn('Erfassung beendet! Daten werden gespeichert ...');
    ReWrite(Bodies); ID:='# '+Beschreib; WriteLn(Bodies,ID);
    WriteLn(Bodies,GSDaten.AnzK);
    WriteLn(Bodies,JulDat);
    FOR Lauf:=1 TO GSDaten.AnzK DO
      WITH GSDaten.Koerper[Lauf] DO
      BEGIN
	WriteLn(Bodies,Info^.Beschreib);
	WriteLn(Bodies,Posi.X:27,Posi.Y:27,Posi.Z:27);
	WriteLn(Bodies,Velo.X:27,Velo.Y:27,Velo.Z:27);
	WriteLn(Bodies,Masse:37);
	WriteLn(Bodies,Ord(UpDate));
	WriteLn(Bodies,Info^.Descrip);
	WriteLn(Bodies,Ord(Info^.KoerperTyp));
	WriteLn(Bodies,Info^.ZentKoerp);
	Dispose(Info)
      END;
    Close(Bodies);
    WriteLn('Eingabedaten (''',BodyName,''') erfolgreich erstellt !')
  END;

  PROCEDURE LoadFile;
  VAR Daten:DiskSystem;
      Lauf:Byte;
      Planeten:TSSystem;
  BEGIN
    WriteLn('Datei ',BodyName,' wird eingelesen ...');
    FOR Lauf:=1 TO MaxKoerper DO New(Planeten[Lauf].Info);
    WITH Daten DO
    BEGIN
      AnzK:=0;
      Koerper:=Addr(Planeten);
      DateiName:=BodyName
    END;
    LoadBodies(Daten);
    IF NOT Daten.OK THEN
    BEGIN
      WriteLn('Fehler aufgetaucht! Beenden Sie sofort und analysieren Sie ihn !');
      Exit
    END;
    WITH GSDaten DO
    BEGIN
      AnzK:=Daten.AnzK;
      Koerper:=Planeten
    END
  END;

  PROCEDURE SavePosi(VAR AnzK:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR P:Pointer); Far;
  VAR Lauf:Byte;
  BEGIN
    WriteLn('Bearbeitet: ',AnzTage/I*GSDaten.Faktor*100:4:1,' %');
    WriteLn(Bodies);
    FOR Lauf:=1 TO AnzK DO WITH Koerper[Lauf] DO
    BEGIN
      WriteLn(Bodies,Posi.X:27,Posi.Y:27,Posi.Z:27);
      WriteLn(Bodies,Velo.X:27,Velo.Y:27,Velo.Z:27)
    END
  END;

  PROCEDURE RunBodies;
  VAR Data:TRKInfo;
    PROCEDURE PosKopf(VAR DVar:Text);
    BEGIN
      WriteLn(DVar,'POS 1.0');		{ Datei-ID 'POS 1.0' }
      WriteLn(DVar,JulDat:10:1);	{ Julianisches Datum zu Beginn }
      WriteLn(DVar,Data.Tage)		{ Anzahl der erfa·ten Tage }
    END;
  BEGIN
    WITH GSDaten DO
    BEGIN
      Data.AnzK:=AnzK;
      Data.Koerper:=Koerper;
      Data.Faktor:=Faktor;
      Write('Wieviele Tage sollen berechnet werden (Schrittweite:',Faktor:7:1,'s) ? ');
      ReadLn(AnzTage); Data.Tage:=AnzTage;
      Data.I2:=Round(SekproTag/Faktor);
      Data.RegCall:=SavePosi;
      Data.Infos:=Nil
    END;
    Assign(Bodies,POSName); ReWrite(Bodies);
    PosKopf(Bodies);
    WriteLn('Iteration ist gestartet worden !');
    Write('Bearbeitet: ');
    RungeKutta(Data);
    Close(Bodies)
  END;

VAR Wahl:Char;

BEGIN
  Init;
  Write('Daten (e)inlesen oder (k)reieren ? ');
  ReadLn(Wahl); Wahl:=UpCase(Wahl);
  IF Wahl='K' THEN CreatePlanetFile;
  LoadFile;
  Write('Positionsdatei erstellen (J/N) ? ');
  ReadLn(Wahl); Wahl:=UpCase(Wahl);
  Write('Zeit zwischen zwei Iterationsschritten (in Sekunden):');
  ReadLn(GSDaten.Faktor);
  IF Wahl='J' THEN RunBodies;
  WriteLn
END.
