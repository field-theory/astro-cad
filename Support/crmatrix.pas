PROGRAM Create_Matrix;
{$N+,E+}

{
	Generierungsprogramm zur Erzeugung von Matrizen fr das erweiterte
	Verfahren zur L”sung von Diffenrentialgleichungen "DiffMatrix" von
	AstroCAD.
	Version: v0.1a
}

USES Crt,Vectors,AstVerw;

VAR Datei:FILE OF Skalar;
    Name,Beschreibung:String;
    Dimension:Skalar;
    Dim:Integer;
    Descrip:ARRAY[1..10] OF Skalar ABSOLUTE Beschreibung;
    Matrix:TMatrix;
    Lauf,Lauf2,PosiY:Byte;

  PROCEDURE LiesZeile(VAR Wert:Skalar);
  VAR Zeile:String;
      a,b:Skalar;
      stelle,code:Integer;
  BEGIN
    Zeile:='';
    ReadLn(Input,Zeile);
    stelle:=Pos('/',Zeile);
    IF stelle>0 THEN
    BEGIN
      Val(Copy(Zeile,1,stelle-1),a,code);
      Val(Copy(Zeile,stelle+1,Length(Zeile)-stelle+1),b,code);
      Wert:=a/b
    END
    ELSE
      Val(Zeile,Wert,code)
  END;

BEGIN
  DirectVideo:=FALSE;
  TextMode(CO80+Font8x8);
  WriteLn('Matrix v0.1a');
  WriteLn('(c) 1992 by Wolfram Schroers');
  WriteLn;
  Write('Dimension der Matrix: '); ReadLn(Dim);
  Write('Name der Datei zum Speichern: '); ReadLn(Name);
  WriteLn('Kurzer Beschreibungstext (max. 249 Zeichen):');
  ReadLn(Beschreibung);
  WriteLn;
  WriteLn('Eingabe der Matrix; bitte beachten Sie, daá die erste Spalte fr');
  WriteLn('die Variation der x-Daten gebraucht wird (in diesem Fall also');
  WriteLn('berflssig ist). Sie darf nicht weggelassen werden, kann aber');
  WriteLn('mit Nullen gefllt werden!');
  WriteLn('Brche k”nnen als ''a/b'' geschrieben werden!');
  WriteLn;
  WriteLn('  x         b1        b2        b3        b4        b5        ...');
  PosiY:=WhereY;
  FOR Lauf:=1 TO Dim DO
  BEGIN
    FOR Lauf2:=1 TO Lauf DO
    BEGIN
      IF Lauf2>6 THEN GotoXY(PosiY,62)
	ELSE GotoXY((Lauf2-1)*10+2,PosiY);
      LiesZeile(Matrix[Lauf,Lauf2])
    END;
    Inc(PosiY)
  END;
  WriteLn; WriteLn;
  WriteLn('Bitte die Ergebnisspalte eingeben:');
  FOR Lauf:=1 TO Dim DO
    LiesZeile(Matrix[Dim+1,Lauf]);
  WriteLn;
  WriteLn('Die Datei wird angelegt ...');
  Assign(Datei,Name); ReWrite(Datei);
  FOR Lauf:=1 TO 10 DO Write(Datei,Descrip[Lauf]);
  Dimension:=Dim;
  Write(Datei,Dimension);
  FOR Lauf:=1 TO MaxMatrix DO
    FOR Lauf2:=1 TO MaxMatrix+1 DO
      Write(Datei,Matrix[Lauf2,Lauf]);
  Close(Datei);
  WriteLn('Fertig !')
END.