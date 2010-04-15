{$DEFINE Kopro}
{$DEFINE DiffMatrix}
UNIT AstVerw;


{
  ***************************************************************************
  *****									*****
  *****			    A S T V E R W . P A S			*****
  *****									*****
  *****			(c) 1991,92 by Wolfram Schroers			*****
  *****									*****
  ***************************************************************************

  Diese UNIT Åbernimmt die Verwaltungsaufgaben bezÅgl. der Positionsdateien.
  Sie enthÑlt au·erdem Funktionen zur Auswertung der Positionen der Himmels-
  kîrper und verwaltet die Diskettenverarbeitung automatisch.

  Version: 0.9a Beta
}


{$IFDEF Kopro}
{$N+,E+}
{$ELSE}
{$N-,E-}
{$ENDIF}
{S-,R-}


INTERFACE


USES Vectors,AstConv;


TYPE 	   	  DiskSystem 	= RECORD
		    AnzK:Byte;		{ Anzahl Kîrper (Ein-/Ausgabewert) }
		    Koerper:PSSystem;	{ Die Kîrper selbst (Ein-/Ausgabewert) }
		    JulDat:Skalar;	{ Datum }
		    DateiName:String;	{ Schreib-/Lesedatei }
		    Beschreib:String;	{ Kommentartext (fÅr Abspeichern) }
		    OK:Boolean		{ Fehlerfrei eingeladen/gespeichert ? }
		  END;
		  SPhen		= RECORD
		    AnzK:Byte;		{ Anzahl Kîrper des Gesamtsystems }
		    Von,Bis:Byte;	{ Untersuchung von Kîrper Nr.. bis Kîrper Nr.. }
		    Koerper:PSSystem;	{ Zeiger auf die Kîrper selbst }
		    Faktor:Skalar;	{ Auflîsung (grob); Iterationsschrittweite fÅr RK }
		    JulDat:Skalar;	{ Das akt. Julianische Datum }
		    Tage:Skalar;	{ Anzahl Tage, die die Suche durchgefÅhrt werden soll
					  (hier sind auch BrÅche mîglich) }
		    ToDat:Text;		{ Ausgabe der Informationen in diese Textdatei }
		    Bahn:Boolean;	{ Sollen Bahndaten ermittelt werden ? }
		    Konstell:Boolean;	{ Sollen Beobachtungs-Informationen gesucht werden ? }
		    Eclipse:Boolean	{ Sollen Sonnen-/Mondfinsternisse gesucht werden ? }
		  END;
		  PText=^Text;		{ Zeiger auf Textdatei }
		  TBBlock	= RECORD
		    JulDat:Skalar;	{ Speicherblock einer JD/Ort/Impuls-Information }
		    Loc,Vel:TVector
		  END;
		  FBBlock=FILE OF TBBlock;


{
 *** TEIL I:	Prozeduren und Funktionen zur Dateiverwaltung		***
}

  PROCEDURE LoadBodies(VAR Data:DiskSystem);
   { Liest die Kîrper mit den entspr. Informationen ein (siehe Disksystem);
     ACHTUNG: Info mu· vorher initialisiert worden sein ! }

  PROCEDURE SaveBodies(VAR Data:DiskSystem);
   { Das GegenstÅck zu "LoadBodies" - speichert die Kîrper entspr. ab ! }

  PROCEDURE SolarPheno(VAR Data:SPhen);
   { Untersucht das Sonnensystem einen angegebenen Zeitraum auf kosmische
     PhÑnomene, die nÑher charakterisiert werden kînnen (siehe SPhen) }

  PROCEDURE CreateTable(RKInfo:PRKInfo;Num:Byte;VAR JulianDate:Skalar;DVar:PText;TeXTable:Boolean);
   { Erstellt eine Tabelle des Num.ten Kîrpers fÅr Tage Tage, in der die
     jeweilige Position des Kîrpers zum aktuellen Julianischen Datum vermerkt
     ist. Die Ausgabe wird in die Textdatei geschrieben.
     TeXTable gibt an, ob die Tabelle als "TeX"-Befehlssatz ausgegeben werden
     soll. }

  PROCEDURE Identify_body(VAR Koerper:TSSystem;Nummer:Byte;JulDat:Skalar);
   { Gibt alle verfÅgbaren Informationen Åber den Kîrper Nummer auf dem
     Bildschirm aus. }

  PROCEDURE Save_body(VAR Koerper:TSSystem;Nummer:Byte;JulianDate:Skalar);
   { Speichert den Ort und den Impuls (Posi und Velo-Vektoren) in einer Datei,
     deren Namen gleich den ersten 8 Zeichen von "Beschreib"+".POS" ist unter
     Angabe des Julianischen Datums. Existiert die Datei noch nicht, wird sie
     angelegt }

  PROCEDURE Load_body(VAR Koerper:TSSystem;Nummer:Byte;VAR JulianDate:Skalar;VAR Found:Boolean);
   { LÑdt eine NEUE Position eines (schon geladenen !!!) Kîrpers von Diskette;
     Wird die richtige Stelle nicht gefunden, so ist Found auf FALSE gesetzt.
     Stattdessen wird der nÑchste Nachbar geladen }

  PROCEDURE Save_matrix(VAR Matrix:TMatrix;Dim:Byte;Name,Beschreib:String);
   { Speichert die Datenmatrix fÅr das Iterationsverfahren ab. Achtung:
     Die Matrix wird immer komplett gespeichert; die korrekte Behandlung
     von "Dim" als Dimension der Matrix ("Dim+1" ist letzte Zeile!) ist
     sowohl beim Speichern als auch beim Laden notwendig! }

  PROCEDURE Load_matrix(VAR Matrix:TMatrix;VAR Dim:Byte;Name:String;VAR Beschreib:String);
   { Das GegenstÅck zu "Save_matrix". Bitte Anmerkung beachten! }


IMPLEMENTATION


{
 *** TEIL I:	Prozeduren und Funktionen zur Dateiverwaltung ***
}


  PROCEDURE LoadBodies(VAR Data:DiskSystem);
  LABEL Beenden;
  VAR DVar:Text;
      Lauf,Lauf2:Byte;
      Zeile:String;
      Convert:Integer;
      RAnz:Byte;
      RJulDat:Skalar;
      Schon_gehabt:Boolean;
      Hilf:Byte;
    FUNCTION Naechste_Zeile:String;
    VAR Zeile:String;
    BEGIN
      REPEAT
	ReadLn(DVar,Zeile)
      UNTIL (Zeile[1]<>'#') OR EOF(DVar);
      Naechste_Zeile:=Zeile
    END;
  BEGIN
    WITH Data DO
    BEGIN
      OK:=TRUE;
      Assign(DVar,DateiName);
      {$I-} Reset(DVar); {$I+}
      IF IOResult<>0 THEN
      BEGIN
	WriteLn('Diskettenfehler !');
	OK:=FALSE;
	Goto Beenden
      END;
      Zeile:=Naechste_Zeile;
      Val(Zeile,RAnz,Convert);
      IF Convert<>0 THEN BEGIN OK:=FALSE; Goto Beenden END;
      Zeile:=Naechste_Zeile;
      Val(Zeile,RJulDat,Convert);
      IF Convert<>0 THEN BEGIN OK:=FALSE; Goto Beenden END;
      IF RJulDat<>JulDat THEN
      BEGIN
	WriteLn('Fehler - unterschiedliches Julianisches Datum !');
	WriteLn('Ladevorgang wird unter Vorbehalt weiterdurchgefÅhrt !')
      END;
      FOR Lauf:=1 TO RAnz DO WITH Koerper^[Lauf] DO
      BEGIN
	Zeile:=Naechste_Zeile;
	Schon_gehabt:=FALSE;
	FOR Lauf2:=1 TO AnzK DO
	  IF Koerper^[Lauf2].Info^.Beschreib=Zeile THEN Schon_gehabt:=TRUE;
		(* Nur neue Kîrper einlesen *)
	IF NOT Schon_gehabt THEN
	BEGIN
          UpDate:=Aktiv; Status:=Aktiv;
	  AnzK:=AnzK+1;
	  Info^.Beschreib:=Zeile;
		(* Kîrper einlesen (ACHTUNG: KEINE KOMMENTARZEILEN) *)
	  ReadLn(DVar,Posi.X,Posi.Y,Posi.Z);
	  ReadLn(DVar,Velo.X,Velo.Y,Velo.Z);
	  ReadLn(DVar,Masse);
	  ReadLn(DVar,Zeile);	{ Boolean einlesen }
		Val(Zeile,Hilf,Convert);
		UpDate:=Boolean(Hilf);
	  ReadLn(DVar,Info^.Descrip);
	  ReadLn(DVar,Zeile);	{ TKoerperTyp einlesen }
		Val(Zeile,Hilf,Convert);
		Info^.KoerperTyp:=TKoerperTyp(Hilf);
	  ReadLn(DVar,Info^.ZentKoerp)
		(* Kîrper eingelesen *)
	END
      END;
      Beenden:Close(DVar)
    END
  END;

  PROCEDURE SaveBodies(VAR Data:DiskSystem);
  VAR Bodies:Text;
      ID:String;
      Lauf:Byte;
  BEGIN
    WITH Data DO
    BEGIN
      OK:=TRUE;
      Assign(Bodies,DateiName);
      ReWrite(Bodies); ID:='# '+Beschreib; WriteLn(Bodies,ID);
      WriteLn(Bodies,AnzK);
      WriteLn(Bodies,JulDat);
      FOR Lauf:=1 TO AnzK DO
	WITH Koerper^[Lauf] DO
	BEGIN
	  WriteLn(Bodies,Info^.Beschreib);
	  WriteLn(Bodies,Posi.X:27,Posi.Y:27,Posi.Z:27);
	  WriteLn(Bodies,Velo.X:27,Velo.Y:27,Velo.Z:27);
	  WriteLn(Bodies,Masse:37);
	  WriteLn(Bodies,Ord(UpDate));
	  WriteLn(Bodies,Info^.Descrip);
	  WriteLn(Bodies,Ord(Info^.KoerperTyp));
	  WriteLn(Bodies,Info^.ZentKoerp)
	END;
      Close(Bodies)
    END
  END;

  PROCEDURE SolarPheno(VAR Data:SPhen);
  TYPE PStatus=^TStatus;
       TStatus=RECORD
		 LastDist:Skalar; { Letzte Distanz zum Ursprungskîrper }
		 LtoAph:Boolean; { Fliegt Kîrper auf Aphel zu ? }
		 Elong:Skalar;	{ Letzte Elongation (geozentr. fÅr innere Planeten) }
		 EltoMax:Boolean; { Fliegt Kîrper auf max. Elong. zu ? }
		 Quadrat:Skalar; { Letztes heliozentr. Skalarprod. Planet * Erde }
		 QtoMax:Boolean { Wird Betrag der Quadratur grî·er ? }
	       END;
  VAR Finster:Boolean;	{ Gerade eine Finsternis? }
      KStatus:ARRAY[1..MaxKoerper] OF PStatus;
      AStatus:ARRAY[1..MaxKoerper] OF PStatus;
      RKInfo:PRKInfo;
      Lauf:Byte;
      IterLen:Skalar;		{ LÑnge eines Iterat.-Schrittes in Tagen }
      FAbst,AFAbst:Skalar;	{ Jeweilige Proj/Schwerpunktdistanz }
      SSizeSun,SSizeMoon:Skalar;
      FTyp,AFStat:TDarkness;
      halb,voll,AHalb,AVoll,BestPhase:Skalar;
    PROCEDURE Init;
    VAR Lauf:Byte;
    BEGIN
      New(RKInfo);
      FOR Lauf:=1 TO Data.AnzK DO
      BEGIN
	New(KStatus[Lauf]); New(AStatus[Lauf])
      END;
      WITH RKInfo^ DO
      BEGIN
	AnzK:=Data.AnzK;
	Koerper:=Data.Koerper^;
	Faktor:=Data.Faktor;
	IterLen:=Faktor/SekproTag;
	Tage:=IterLen;
	I2:=Round(Data.Faktor);
	RegCall:=Dummy
      END;
      FTyp:=Keine; AFAbst:=AstroUnit;
      WriteLn('Auswertung beginnt ...')
    END;
    PROCEDURE DeInit;
    VAR Lauf:Byte;
    BEGIN
      FOR Lauf:=1 TO Data.AnzK DO
      BEGIN
	Dispose(KStatus[Lauf]); Dispose(AStatus[Lauf])
      END;
      Dispose(RKInfo)
    END;
    PROCEDURE UpDateInfo;
    VAR Hilf,Hilf2:TVector;
	Lauf:Byte;
    BEGIN
      FOR Lauf:=1 TO Data.AnzK DO AStatus[Lauf]^:=KStatus[Lauf]^;
      AFStat:=FTyp;
      WITH RKInfo^ DO
	FOR Lauf:=1 TO AnzK DO IF Lauf<>SonneNr THEN WITH KStatus[Lauf]^ DO
	BEGIN
	  VSub(Hilf,Koerper[Lauf].Posi,Koerper[Koerper[Lauf].Info^.ZentKoerp].Posi);
	  LastDist:=VLen(Hilf); { Letzter Abstand zum Zentralkîrper }
	  IF (Lauf<=SaturnNr) AND (Lauf>MondNr) THEN
	  BEGIN
	    Elong:=Elongation(Koerper,Lauf);
	    VSub(Hilf,Koerper[Lauf].Posi,Koerper[SonneNr].Posi);
	    VSub(Hilf2,Koerper[ErdNr].Posi,Koerper[SonneNr].Posi);
	    Quadrat:=VSProd(Hilf,Hilf2)
	  END
	END;
      WITH RKInfo^ DO Eclipse(AnzK,Koerper,FAbst,halb,voll,FTyp);
      IF (FTyp<>Keine) AND (FAbst<AFAbst) THEN
      BEGIN
	AFAbst:=FAbst;
	AHalb:=halb; AVoll:=voll;
	BestPhase:=Data.JulDat;
	SSizeSun:=SemiDiam(RKInfo^.Koerper,SonneNr);
	SSizeMoon:=SemiDiam(RKInfo^.Koerper,MondNr)
      END
    END;
    PROCEDURE One_step;
    BEGIN
      Data.JulDat:=Data.JulDat+IterLen;
      WriteLn(Data.JulDat:10:5);
      RKInfo^.Tage:=IterLen;
      Data.Tage:=Data.Tage-IterLen;
      RungeKutta(RKInfo^)
    END;
    PROCEDURE FindPheno;
    VAR Lauf:Byte;
      PROCEDURE Zustand_ausgeben(Zusatz:String);
      VAR Tag,Monat,Jahr,Stunden,Minuten:Integer;
      BEGIN
	JDtoUt(Data.JulDat,Tag,Monat,Jahr,Stunden,Minuten);
	WriteLn;
	WriteLn('----======>',(Data.JulDat):10:1,'=',Tag:2,'.',
		Monat:2,'.',Jahr:4,'':2,Stunden:2,':',Minuten:2,'':3,
		Zusatz,' von ',RKInfo^.Koerper[Lauf].Info^.Beschreib);
	WriteLn(Data.ToDat,(Data.JulDat):10:1,
		'=',Tag:2,'.',Monat:2,'.',Jahr:4,'':2,
                 Stunden:2,':',Minuten:2,'':3,
		Zusatz,' von ',RKInfo^.Koerper[Lauf].Info^.Beschreib)
      END;
      PROCEDURE Finsternis_melden(Art:String);
      VAR Tag,Monat,Jahr,Stunden,Minuten:Integer;
      BEGIN
	JDtoUt(Data.JulDat,Tag,Monat,Jahr,Stunden,Minuten);
	WriteLn;
	WriteLn('----======>',(Data.JulDat):10:1,'=',Tag:2,'.',
		Monat:2,'.',Jahr:4,'':2,Stunden:2,':',Minuten:2,'':3,
                Art);
	WriteLn(Data.ToDat,(Data.JulDat):10:1,
		'=',Tag:2,'.',Monat:2,'.',Jahr:4,'':2,
                 Stunden:2,':',Minuten:2,'':3,
                Art)
      END;
    BEGIN
      One_step;
      UpDateInfo;
      FOR Lauf:=1 TO Data.AnzK DO WITH KStatus[Lauf]^ DO
      BEGIN
	IF Data.Bahn THEN
	BEGIN
	  IF (AStatus[Lauf]^.LastDist<LastDist) AND (NOT LtoAph) THEN
	  BEGIN
	    LtoAph:=TRUE;
	    Zustand_ausgeben('Periheldurchgang')
	  END;
	  IF (AStatus[Lauf]^.LastDist>LastDist) AND LtoAph THEN
	    BEGIN
	      LtoAph:=FALSE;
	      Zustand_ausgeben('Apheldurchgang')
	    END
	END;
	IF Data.Konstell THEN
	BEGIN
	  IF (AStatus[Lauf]^.Elong<Elong) AND (NOT EltoMax) THEN
	  BEGIN
	    EltoMax:=TRUE;
	    IF Lauf>=MarsNr THEN
            BEGIN
              IF Sgn(Quadrat)=-1 THEN Zustand_ausgeben('Konjunktion')
                                 ELSE Zustand_ausgeben('Opposition')
            END
	    ELSE IF Sgn(Quadrat)=-1 THEN Zustand_ausgeben('obere Konjunktion')
				    ELSE Zustand_ausgeben('untere Konjunktion')
	  END;
	  IF (AStatus[Lauf]^.ELong>ELong) AND EltoMax THEN
	    BEGIN
	      EltoMax:=FALSE;
	      IF (Lauf<MarsNr) AND (Lauf>MondNr) THEN
		Zustand_ausgeben('max. Elongation')
	      {ELSE Zustand_ausgeben('Opposition')}
	    END;
	  IF (Abs(AStatus[Lauf]^.Quadrat)<Abs(Quadrat)) AND (NOT QtoMax) THEN
	  BEGIN
	    QtoMax:=TRUE;
	    Zustand_ausgeben('Quadratur')
	  END;
	  IF (Abs(AStatus[Lauf]^.Quadrat)>Abs(Quadrat)) AND QtoMax THEN QtoMax:=FALSE
	END
      END;
      IF Data.Eclipse THEN
      BEGIN
	IF (FTyp=Keine) AND (AFStat<>Keine) THEN
	BEGIN
	  WriteLn(Data.ToDat,(Data.JulDat):10:1,': Ende der Finsternis');
	  IF AFStat=SonF THEN
	  BEGIN
	    IF AFAbst<ErdRadius-AVoll THEN
	      IF SSizeSun>SSizeMoon THEN Write(Data.ToDat,'Ringfîrmige')
		ELSE Write(Data.ToDat,'Totale')
	      ELSE Write(Data.ToDat,'Partielle');
	    WriteLn(Data.ToDat,' Sonnenfinsternis !')
	  END
	  ELSE
	  BEGIN
	    IF AFAbst<AVoll-MondRadius THEN Write(Data.ToDat,'Totale')
	      ELSE IF AFAbst<AVoll+MondRadius THEN Write(Data.ToDat,'Partielle')
	      ELSE Write(Data.ToDat,'Halbschatten');
	    WriteLn(Data.ToDat,' Mondfinsternis !')
	  END;
	  WriteLn(Data.ToDat,'----- Einzelheiten:');
	  WriteLn(Data.ToDat,'Halbschattenradius: ',AHalb:1:10);
	  WriteLn(Data.ToDat,'Kernschattenradius: ',AVoll:1:10);
	  WriteLn(Data.ToDat,'KÅrzeste Distanz:   ',AFAbst:1:10);
	  WriteLn(Data.ToDat,'GÅnstigster Zeitpunkt: ',BestPhase:10:1);
	  WriteLn(Data.ToDat,'Scheinb. Sonnenrad: ',SSizeSun:1:10);
	  WriteLn(Data.ToDat,'Scheinb. Mondrad:   ',SSizeMoon:1:10);
	  WriteLn(Data.ToDat,'----- Ende der Analyse');
	  WriteLn(Data.ToDat);
	  AFAbst:=AstroUnit
	END;
	IF (FTyp<>Keine) AND (AFStat=Keine) THEN
	BEGIN
	  WriteLn(Data.ToDat,Data.JulDat:10:1,': Beginn einer Finsternis');
	  Finsternis_melden('Finsternis')
	END
      END
    END;
  BEGIN
    Init;
    UpDateInfo;
    One_step;
    UpDateInfo;
    FOR Lauf:=1 TO Data.AnzK DO WITH KStatus[Lauf]^ DO
    BEGIN
      IF AStatus[Lauf]^.LastDist>LastDist THEN LtoAph:=FALSE
	ELSE LtoAph:=TRUE;
      IF AStatus[Lauf]^.Elong>Elong THEN EltoMax:=FALSE
	ELSE EltoMax:=TRUE;
      IF Abs(AStatus[Lauf]^.Quadrat)>Abs(Quadrat) THEN QtoMax:=FALSE
	ELSE QtoMax:=TRUE
    END;
    WHILE Data.Tage>0 DO FindPheno;
    DeInit
  END;

  TYPE TDaten_fuer_TE = RECORD
			  DatVar:PText;
			  JulDat:Skalar;
			  KoerperNummer:Byte;
			  TTable:Boolean
			END;

  {*** FÅr diese Funktion globale Definitionen: ***}
  VAR Epsilon,Majaxis,Distance,Angle:Text;
      L,r,rp,Hilf2:TVector;
      P,axis,e,Winkel:Skalar;

    PROCEDURE TableEntry(VAR A:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR PDaten:Pointer); Far;
    VAR Desc:Integer;
	Hilf:TVector;
	el,eb,dist:Skalar;
	grad,minuten,sekunden:Skalar;
	Tag,Monat,Jahr,Stunden,Minut:Integer;
    BEGIN
      WITH TDaten_fuer_TE(PDaten^) DO
	WITH Koerper[KoerperNummer] DO
	BEGIN
	  JulDat:=JulDat+1;
{---===>} WriteLn(JulDat:1:1);
          JDtoUT(JulDat,Tag,Monat,Jahr,Stunden,Minut);
	  Desc:=Info^.Descrip;
	  IF TTable THEN Write(DatVar^,JulDat:10:1,'&')
		    ELSE Write(DatVar^,Tag:2,'.',Monat:2,'.',Jahr,'&');
	  IF BitSet(Desc,2) THEN VSub(Hilf,Posi,Koerper[ErdNr].Posi)
			    ELSE VSub(Hilf,Posi,Koerper[SonneNr].Posi);
	  WITH Hilf DO VectoElB(X,Y,Z,el,eb,dist);
	  IF NOT BitSet(Desc,0) THEN
	  BEGIN
	    ElBtoRaD(el,eb,el,eb,JulDat);
	    DegtoRA(el,grad,minuten,sekunden)
	  END
	  ELSE DegtoHMS(el,grad,minuten,sekunden);
	  IF TTable THEN Write(DatVar^,grad:4:0,'&',minuten:4:0,'&',sekunden:9:3,'&')
		    ELSE Write(DatVar^,grad:4:0,minuten:4:0,sekunden:8:3,'  ');
	  DegtoHMS(eb,grad,minuten,sekunden);
	  IF TTable THEN Write(DatVar^,grad:4:0,'&',minuten:4:0,'&',sekunden:9:3,'&')
		    ELSE Write(DatVar^,grad:4:0,minuten:4:0,sekunden:8:3,'  ');
	  IF BitSet(Desc,1) THEN dist:=dist/ErdRadius
			    ELSE dist:=dist/AstroUnit;
	  IF TTable THEN WriteLn(DatVar^,dist:12:9,'\\')
		    ELSE Write(DatVar^,dist:12:9);
	  dist:=SemiDiam(Koerper,KoerperNummer);
	  DegtoHMS(dist,grad,minuten,sekunden);
	  IF NOT TTable THEN WriteLn(DatVar^,grad:4:0,minuten:4:0,sekunden:8:3);

	  {*** Zusatz fÅr das Celestial Mechanics Projekt: ***}

	  (* VSub(r,Koerper[MondNr].Posi,Koerper[ErdNr].Posi);
	  VSub(rp,Koerper[MondNr].Velo,Koerper[ErdNr].Velo);
	  VCProd(L,r,rp);
	  VSMult(L,Koerper[MondNr].Masse);
	  P:=Sqr(VLen(L))/f/Koerper[ErdNr].Masse/Sqr(Koerper[MondNr].Masse);
	  axis:=1/(2/VLen(r)-VLen(rp)*VLen(rp)/f/Koerper[ErdNr].Masse);
	  e:=Sqrt(1-P/axis);
	  VSub(Hilf,Koerper[MondNr].Posi,Koerper[ErdNr].Posi);
	  VSub(Hilf2,Koerper[SonneNr].Posi,Koerper[ErdNr].Posi);
	  VecNorm(Hilf); VecNorm(Hilf2);
	  Winkel:=RadtoDeg(ArcCos(VSProd(Hilf,Hilf2)));

	  WriteLn(Epsilon,e);
	  WriteLn(Majaxis,axis);
	  WriteLn(Distance,VLen(r));
	  WriteLn(Angle,Winkel); *)

	END
    END;
  PROCEDURE CreateTable(RKInfo:PRKInfo;Num:Byte;VAR JulianDate:Skalar;DVar:PText;TeXTable:Boolean);
  VAR MDim,Lauf,Lauf2:Byte;
      Matrix:TMatrix;
      DfTE:TDaten_fuer_TE;
      Descrip:String;
  BEGIN
    {*** ErgÑnzungen fÅr die Analysen: ***}
    (* Assign(Epsilon,'EPSILON.ACD'); ReWrite(Epsilon);
    Assign(Majaxis,'MAJAXIS.ACD'); ReWrite(Majaxis);
    Assign(Distance,'DISTANCE.ACD'); ReWrite(Distance);
    Assign(Angle,'ANGLE.ACD'); ReWrite(Angle); *)

    WriteLn(DVar^,RKInfo^.Koerper[Num].Info^.Beschreib);
    WriteLn(DVar^);
    IF TeXTable THEN
    BEGIN
      WriteLn(DVar^,'\begin{tabular}{l|lll|lll|l} \\');
      WriteLn(DVar^,'Jul. Datum &');
      Write(DVar^,'\multicolumn{3}{|c|}{');
      IF BitSet(RKInfo^.Koerper[Num].Info^.Descrip,0) THEN
      BEGIN
	WriteLn(DVar^,'Ekliptikale L"ange} &');
	WriteLn(DVar^,'\multicolumn{3}{|c|}{Ekliptikale Breite} &')
      END
      ELSE
      BEGIN
	WriteLn(DVar^,'Rektaszension} &');
	WriteLn(DVar^,'\multicolumn{3}{|c|}{Deklination} &')
      END;
      WriteLn(DVar^,'Radiusvektor \\ \hline')
    END
    ELSE
    BEGIN
      Write(DVar^,'Jul.Datum':10);
      IF BitSet(RKInfo^.Koerper[Num].Info^.Descrip,0) THEN
	Write(DVar^,'Eklip. LÑnge':15,'Eklip. Breite':15)
	ELSE Write(DVar^,'Rektaszension':18,'Deklination':18);
      WriteLn(DVar^,'':8,'Rad.-Vektor  Scheinb. Radius');
    END;
    WITH DfTE DO
    BEGIN
      Move(DVar,DatVar,SizeOf(DVar));
      JulDat:=JulianDate;
      KoerperNummer:=Num;
      TTable:=TeXTable
    END;
    RKInfo^.RegCall:=TableEntry;
    RKInfo^.Infos:=Addr(DfTE);

{$IFNDEF DiffMatrix}
    WriteLn('Verwendung des klassischen Verfahrens ... ');
    RungeKutta(RKInfo^);

{$ELSE}
    (* ACHTUNG: DAS NEUE VERFAHREN *)
    WriteLn('Verwendung des neuen Verfahrens ... ');
    (* FOR Lauf:=1 TO 9 DO FOR Lauf2:=1 TO 8 DO Matrix[Lauf,Lauf2]:=0;
    Matrix[2,1]:=1/2; Matrix[2,2]:=1/2;
    Matrix[3,1]:=1/2; Matrix[3,3]:=1/2;
    Matrix[4,1]:=1;   Matrix[4,4]:=1;
    Matrix[5,1]:=1/6; Matrix[5,2]:=1/3; Matrix[5,3]:=1/3; Matrix[5,4]:=1/6;
    MDim:=4; *)
    Load_matrix(Matrix,MDim,'RKFehlb.Mat',Descrip);
    DiffMatrix(RKInfo^,Matrix,MDim);
{$ENDIF}

    JulianDate:=DfTE.JulDat;
    Move(DfTE.DatVar,DVar,SizeOf(DVar));
    IF TeXTable THEN Writeln(DVar^,'\end{tabular}');
    WriteLn(DVar^);

    {*** Schlie·en der Zusatzdateien ***}
    (* Close(Epsilon);
    Close(Majaxis);
    Close(Distance);
    Close(Angle); *)

  END;

  PROCEDURE Identify_body(VAR Koerper:TSSystem;Nummer:Byte;JulDat:Skalar);
  VAR Desc:Integer;
      Hilf:TVector;
      el,eb,dist,geschw:Skalar;
      grad,minuten,sekunden:Skalar;
    PROCEDURE Zeige_Winkel(Wert:Skalar;RA:Boolean);
    BEGIN
      IF RA THEN
	    BEGIN
	      DegtoRA(Wert,grad,minuten,sekunden);
	      Write(grad:2:0,'h',minuten:3:0,'m',sekunden:7:3,'s')
	    END
	    ELSE
	    BEGIN
	      DegtoHMS(Wert,grad,minuten,sekunden);
	      Write(grad:3:0,'¯',minuten:3:0,'''',sekunden:7:3,'''''')
	    END;
      WriteLn(' (=',Wert:12:8,'¯)')
    END;
  BEGIN
    WITH Koerper[Nummer] DO
    BEGIN
	(* Basis-Informationen ausgeben *)
      WriteLn;
      WriteLn('Kîrper Nr. #',Nummer:2,': ',Info^.Beschreib);
      Write('Typ: ');
      CASE Info^.KoerperTyp OF
	Sonne		: Write('Sonne');
	Planet		: Write('Planet');
	Mond		: Write('Mond');
	Asteroid	: Write('Asteroid');
       ELSE Write('Komet')
      END;
      IF Info^.ZentKoerp>0 THEN
	WriteLn(', kreisend um Kîrper #',Info^.ZentKoerp,' (',
		 Koerper[Info^.ZentKoerp].Info^.Beschreib,')');
      Write('Masse: ',Masse:12,' kg ');
      IF (Nummer<PlutoNr) AND (Nummer<>SonneNr) THEN
	IF Nummer=MondNr THEN Write('(=',PlanetenMasse[MondNr],' Erdmassen)')
	ELSE Write('(=1/',PlanetenMasse[Nummer]:10:2,' Sonnenmassen)');
      WriteLn;
      IF Nummer<=PlutoNr THEN
	WriteLn('Radius des Kîrpers: ',PRadius[Nummer]/1000:1:3,'km');
      WriteLn('Schwerkraft an der OberflÑche: ',
	       f*Masse/Sqr(PRadius[Nummer])/Erd_g:1:3,'g (=...*',Erd_g:1:3,'m/s^2)');
      WriteLn('Fluchtgeschwindigkeit an der OberflÑche: v=',
	       Sqrt(f*Masse/PRadius[Nummer]):1:3,'m/s');
	(* Ein-/Ausgabeformatierung festlegen *)
      Desc:=Info^.Descrip;
      Write('IO-Code: ',Desc:3,' (');
      IF BitSet(Desc,0) THEN Write('Ekliptik,')
			ELSE Write('HimmelsÑquator,');
      IF BitSet(Desc,1) THEN Write(' Radvektor in ER,')
			ELSE Write(' Radvektor in AU,');
      IF BitSet(Desc,2) THEN Write(' geozentr.,')
			ELSE Write(' heliozentr.,');
      IF BitSet(Desc,3) THEN Write(' Erdstatus,');
      IF BitSet(Desc,4) THEN Write(' Radvektor in Parallaxe,');
      WriteLn(')');
	(* Position ausgeben *)
      IF NOT BitSet(Desc,3) THEN
      BEGIN
	IF BitSet(Desc,2) THEN VSub(Hilf,Posi,Koerper[ErdNr].Posi)
			  ELSE VSub(Hilf,Posi,Koerper[SonneNr].Posi);
	WITH Hilf DO VectoElB(X,Y,Z,el,eb,dist);
	IF NOT BitSet(Desc,0) THEN
	BEGIN
	  ElBtoRaD(el,eb,el,eb,JulDat);
	  Write('Rektaszension: '); Zeige_Winkel(el,TRUE);
	  Write('Deklination:   '); Zeige_Winkel(eb,FALSE)
	END
	ELSE
	BEGIN
	  Write('Ekliptikale LÑnge:  '); Zeige_Winkel(el,FALSE);
	  Write('Ekliptikale Breite: '); Zeige_Winkel(eb,FALSE)
	END;
	Write('TÑgliche Parallaxe: ');
	Zeige_Winkel(Parallax(Koerper,Nummer),FALSE);
	Write('Scheinbarer geozentr. Radius: ');
	Zeige_Winkel(Semidiam(Koerper,Nummer),FALSE);
	WriteLn('Lichtlaufzeit bis zur Erde: ',
		 LightTime(Koerper,ErdNr,Nummer,1),' s');
	IF BitSet(Desc,2) THEN Write('Geozentrische') ELSE Write('Heliozentrische');
	Write(' Distanz: ');
	IF BitSet(Desc,1) THEN WriteLn(dist/ErdRadius:10:7,' Erd-Radien')
			  ELSE WriteLn(dist/AstroUnit:10:7,' Astronomische Einheiten');
      END
      ELSE WriteLn('Wir stehen gerade darauf !');
	(* Geschwindigkeit ausgeben *)
      WriteLn('Die aktuelle Geschwindigkeit des Kîrpers betrÑgt:');
      WriteLn(VLen(Velo)/1000:12:8,' km/s in bezug auf den Schwerpunkt des Systems');
      IF Info^.KoerperTyp<>Sonne THEN
      BEGIN
	VSub(Hilf,Velo,Koerper[Info^.ZentKoerp].Velo);
	WriteLn(VLen(Hilf)/1000:12:8,' km/s in bezug auf seinen Zentralkîrper')
      END;
      IF (Nummer<>ErdNr) THEN
      BEGIN
	VSub(Hilf,Velo,Koerper[ErdNr].Velo);
	WriteLn(VLen(Hilf)/1000:12:8,' km/s in bezug auf die Erde')
      END;
	(* Zustandsvariablen ausgeben *)
      Write('Das UpDate-Flag ist ');
      IF UpDate=NichtAktiv THEN Write('nicht ');
      WriteLn('aktiviert');
      Write('Der Status des Kîrpers ist ');
      IF Status=NichtAktiv THEN Write('nicht ');
      WriteLn('aktiviert');
    END
  END;

  PROCEDURE Save_body(VAR Koerper:TSSystem;Nummer:Byte;JulianDate:Skalar);
  VAR DiskName,TempName:String[8];
      Datei,Temp:FBBlock;
      Satz:TBBlock;
      GefDat:Skalar;
    PROCEDURE SpeicherBlock(VAR Datei:FBBlock);
    BEGIN
      WITH Satz DO
      BEGIN
	JulDat:=JulianDate;
	Loc:=Koerper[Nummer].Posi;
	Vel:=Koerper[Nummer].Velo
      END;
      Write(Datei,Satz)
    END;
  BEGIN
    DiskName:=Koerper[Nummer].Info^.Beschreib;
    IF Length(DiskName)>8 THEN DiskName:=Copy(DiskName,1,8);
    TempName:=DiskName+'.$$$'; DiskName:=DiskName+'.Pos';
    Assign(Datei,DiskName);
    {$I-} Reset(Datei); {$I+}
    IF IOResult<>0 THEN
    BEGIN
      ReWrite(Datei);
      SpeicherBlock(Datei);
      Close(Datei)
    END
    ELSE
      BEGIN
	Assign(Temp,TempName);
	ReWrite(Temp);
	Read(Datei,Satz);
	WHILE (Satz.JulDat<JulianDate) AND (NOT Eof(Datei)) DO
	BEGIN
	  Read(Datei,Satz);
	  Write(Temp,Satz)
	END;
	SpeicherBlock(Temp);
	WHILE NOT Eof(Datei) DO
	BEGIN
	  Read(Datei,Satz);
	  Write(Temp,Satz)
	END;
	Close(Datei);
	Erase(Datei);
	Rename(Temp,DiskName);
	Close(Temp)
      END
  END;

  PROCEDURE Load_body(VAR Koerper:TSSystem;Nummer:Byte;VAR JulianDate:Skalar;VAR Found:Boolean);
  VAR DiskName:String[8];
      Datei:FBBlock;
      Satz:TBBlock;
  BEGIN
    DiskName:=Koerper[Nummer].Info^.Beschreib;
    IF Length(DiskName)>8 THEN DiskName:=Copy(DiskName,1,8);
    DiskName:=DiskName+'.Pos';
    Found:=FALSE;
    Assign(Datei,DiskName);
    {$I-} Reset(Datei); {$I+}
    IF IOResult<>0 THEN Exit;
    Read(Datei,Satz);
    WHILE (Satz.JulDat<JulianDate) AND (NOT Eof(Datei)) DO
      Read(Datei,Satz);
    Koerper[Nummer].Posi:=Satz.Loc;
    Koerper[Nummer].Velo:=Satz.Vel;
    IF JulianDate=Satz.JulDat THEN Found:=TRUE ELSE JulianDate:=Satz.JulDat;
    Close(Datei)
  END;

  PROCEDURE Save_matrix(VAR Matrix:TMatrix;Dim:Byte;Name,Beschreib:String);
  VAR Datei:FILE OF Skalar;
      Dimension:Skalar;
      Descrip:ARRAY[1..10] OF Skalar ABSOLUTE Beschreib;
      Lauf,Lauf2:Byte;
  BEGIN
    Assign(Datei,Name); ReWrite(Datei);
    FOR Lauf:=1 TO 10 DO
      Write(Datei,Descrip[Lauf]);
    Dimension:=Dim;
    Write(Datei,Dimension);
    FOR Lauf:=1 TO MaxMatrix DO
      FOR Lauf2:=1 TO MaxMatrix+1 DO
	Write(Datei,Matrix[Lauf2,Lauf]);
    Close(Datei)
  END;

  PROCEDURE Load_matrix(VAR Matrix:TMatrix;VAR Dim:Byte;Name:String;VAR Beschreib:String);
  VAR Datei:FILE OF Skalar;
      Dimension:Skalar;
      Descrip:ARRAY[1..10] OF Skalar ABSOLUTE Beschreib;
      Lauf,Lauf2:Byte;
  BEGIN
    Assign(Datei,Name); Reset(Datei);
    FOR Lauf:=1 TO 10 DO
      Read(Datei,Descrip[Lauf]);
    Read(Datei,Dimension);
    Dim:=Round(Dimension);
    FOR Lauf:=1 TO MaxMatrix DO
      FOR Lauf2:=1 TO MaxMatrix+1 DO
	Read(Datei,Matrix[Lauf2,Lauf]);
    Close(Datei)
  END;


BEGIN
END.
