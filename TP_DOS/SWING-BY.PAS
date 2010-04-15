PROGRAM Swing_by;


{$N+,E+}
{$X+}

USES AstConv,Vectors,AstVerw,Graph,Crt{,ScrCopy};

{
 ****************************************
 ***				      ***
 ***	  S W I N G - B Y . P A S     ***
 *** Simulation des Swing-by-effektes ***
 ***   (c) 1992 by Wolfram Schroers   ***
 ***				      ***
 ****************************************
}


{
	*** TEIL I: Globale Variablen und Konstanten ***
}


CONST ZoomFactor=350;
      TagdFaktor=200;


VAR RKInfo:TRKInfo;
    VersX,VersY:Integer;
    Datei:Text;
    Zoom:Skalar;
    JupiterRad:Skalar;
    d_min:Skalar;


{
	*** TEIL II: Prozeduren und Funktionen ***
}


  PROCEDURE PlotStar(x,y,BSize:Integer;Col,Float,RayL:Byte;NebenMax:Boolean);
  VAR HalfR,HalfB:Integer;
  BEGIN
    IF RayL>0 THEN
    BEGIN
      IF Float>0 THEN
      BEGIN
	HalfR:=Round(RayL/1.5);
	HalfB:=Round(BSize/2);
	SetColor(Col+(Float DIV 2));
	Line(x+HalfB,y+HalfB,x+HalfR,y);
	Line(x+HalfR,y,x+HalfB,y-HalfB);
	Line(x+HalfB,y-HalfB,x,y-HalfR);
	Line(x,y-HalfR,x-HalfB,y-HalfB);
	Line(x-HalfB,y-HalfB,x-HalfR,y);
	Line(x-HalfR,y,x-HalfB,y+HalfB);
	Line(x-HalfB,y+HalfB,x,y+HalfR);
	Line(x,y+HalfR,x+HalfB,y+HalfB);
	MoveTo(x,y);
	SetFillStyle(SolidFill,Col+(Float DIV 2));
	FloodFill(x,y,Col+(Float DIV 2))
      END;
      SetColor(Col);
      Line(x-RayL,y,x+RayL,y);
      Line(x,y+RayL,x,y-RayL)
    END;
    IF NebenMax THEN
    BEGIN
      SetColor(Col);
      Circle(x,y,BSize+Float*3);
      IF Float>0 THEN
      BEGIN
	SetColor(Col+1);
	Circle(x,y,BSize+Float*3+1);
	Circle(x,y,BSize+Float*3-1)
      END
    END;
    WHILE Float>0 DO
    BEGIN
      SetColor(Col+Float);
      SetFillStyle(SolidFill,Col+Float);
      PieSlice(x,y,0,360,BSize+Float-1);
      Dec(Float)
    END
  END;

  PROCEDURE GetDMin(VAR Koerper:TSSystem);
  VAR Dist:TVector;
  BEGIN
    VSub(Dist,Koerper[3].Posi,Koerper[2].Posi);
    IF d_min>VLen(Dist)/JupiterRad THEN d_min:=VLen(Dist)/JupiterRad
  END;

  PROCEDURE Status(VAR A:Byte;VAR Koerper:TSSystem;VAR I:LongInt;VAR Leer:Pointer);
  Far;
  VAR Speed:Skalar;
      ScrX2,ScrY2,ScrX3,ScrY3:Integer;
      Zeichen:String;
      Distance:TVector;
  BEGIN
    GetDMin(Koerper);
    IF I MOD (TagdFaktor*4)=0 THEN
    BEGIN
      ScrX2:=VersX+Round((Koerper[2].Posi.X-5*AstroUnit)*Zoom);
      ScrY2:=VersY-Round(Koerper[2].Posi.Y*Zoom);
      ScrX3:=VersX+Round((Koerper[3].Posi.X-5*AstroUnit)*Zoom);
      ScrY3:=VersY-Round(Koerper[3].Posi.Y*Zoom);
      PutPixel(ScrX2,ScrY2,Red);
      PutPixel(ScrX3,ScrY3,LightGray);
      GotoXY(10,5);
      Speed:=VLen(Koerper[3].Velo);
      Write(Speed:1:3);
      GotoXY(10,6);
      Write(d_min:1:3);
      {WriteLn(Datei,Int(i/TagdFaktor):1:0,'&',Speed:1:3,'\\');}
      IF Speed>Sqrt(2*f*SonnenMasse/VLen(Koerper[3].Posi)) THEN
      BEGIN
        Sound(500);
        Delay(10);
        NoSound
      END;
      IF Trunc(i/TagdFaktor) MOD 20=0 THEN
      BEGIN
        WITH Koerper[2] DO
        BEGIN
	  SetColor(Red);
	  Line(ScrX2-5,ScrY2,ScrX2+5,ScrY2);
	  Str(Int(i/TagdFaktor):1:0,Zeichen);
	  OutTextXY(ScrX2+10,ScrY2-5,Zeichen)
        END;
        WITH Koerper[3] DO
        BEGIN
	  SetColor(LightGray);
	  Line(ScrX3,ScrY3-5,ScrX3,ScrY3+5);
	  OutTextXY(ScrX3-4,ScrY3-15,Zeichen)
        END
      END
    END
  END;

  PROCEDURE Init;
  VAR gd,gm:Integer;
  BEGIN
    gd:=Detect;
    InitGraph(gd,gm,'C:\TP7\BGI');
    IF GraphResult<>0 THEN
    BEGIN WriteLn('Fehler: Grafiktreiber nicht gefunden !'); Halt END;
    DirectVideo:=NichtAktiv; CheckSnow:=NichtAktiv;
    Zoom:=ZoomFactor/AstroUnit;
    ClearDevice;
    GotoXY(1,5); Write(' v Akt =');
    GotoXY(1,6); Write(' d min =');
    SetTextStyle(TriplexFont,HorizDir,4);
    OutTextXY(5,5,'Swing-by Manîver');
    SetTextStyle(SmallFont,HorizDir,4);
    VersX:=GetMaxX DIV 2; VersY:=GetMaxY DIV 2+150;
    PlotStar(VersX-Round(5*AstroUnit*Zoom),VersY,3,Yellow,1,7,NichtAktiv);
    Assign(Datei,'SwB1.CAD'); ReWrite(Datei);
    WITH RKInfo DO
    BEGIN
      AnzK:=3;
      WITH Koerper[SonneNr] DO			{ *** SONNE *** }
      BEGIN
	Posi:=NullVector;
	Velo:=NullVector;
	Masse:=SonnenMasse;
	Status:=Aktiv;
	UpDate:=Aktiv;
        Info:=Nil
      END;
      WITH Koerper[2] DO			{ *** PLANET=JUPITER *** }
      BEGIN
	Posi:=NullVector;
	Posi.X:=5*AstroUnit;
	Velo:=NullVector;
	Velo.Y:=Sqrt(f*SonnenMasse/5/AstroUnit);
	Masse:=SonnenMasse/PlanetenMasse[7];
	Status:=Aktiv;
	UpDate:=Aktiv;
	Info:=Nil
      END;
      WITH Koerper[3] DO			{ *** RAUMSCHIFF *** }
      BEGIN
	Posi:=NullVector;
	Posi.X:=4.5*AstroUnit;	{ ...=4.48 * AstroUnit fÅr Variante 1 }
	Posi.Y:=0.5*AstroUnit;
	Velo:=NullVector;
	Velo.X:=0.7*Sqrt(2*f*SonnenMasse/4.5/AstroUnit);
	Masse:=1000; { Gewicht: 1 Tonne }
	Status:=Aktiv;
	UpDate:=Aktiv;
	Info:=Nil
      END;
      Faktor:=SekproTag/TagdFaktor;
      Tage:=100;
      I2:=1{Round(SekproTag/Faktor)*4};
      RegCall:=Status;
      Infos:=Nil;
      JupiterRad:=PRadius[MarsNr+1];
      d_min:=AstroUnit/JupiterRad;
      Status(AnzK,Koerper,I2,Infos)
    END
  END;

  PROCEDURE Lauf;
  BEGIN
    RungeKutta(RKInfo)
  END;

  PROCEDURE DeInit;
  BEGIN
    SetTextStyle(SmallFont,HorizDir,5);
    OutTextXY(325,340,'Jupiterbahn');
    OutTextXY(430,120,'Raumschiffbahn');
    WriteLn(' (vor Jupiter)');
    WriteLn(' v Fl = ',Sqrt(2*f*SonnenMasse/VLen(RKInfo.Koerper[3].Posi)):10:3);
    WriteLn(' v St = ',0.7*Sqrt(2*f*SonnenMasse/4.5/AstroUnit):10:3);
    SetColor(LightGreen);
    Line(1,1,639,1);
    Line(639,1,639,440);
    Line(639,440,1,440);
    Line(1,440,1,1);
    Close(Datei);
    ReadKey;
    {HardCopy(FALSE);}
    CloseGraph
  END;


{
	*** TEIL III: Hauptprogramm ***
}


BEGIN
  Init;
  Lauf;
  DeInit
END.
