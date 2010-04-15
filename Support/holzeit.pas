PROGRAM Hole_Sternzeit;

USES Vectors,AstConv;

VAR h,m,s,Jahr,Monat,Tag:Integer;
    MOZ,MEZ,SRT,JD,Laenge:Skalar;

BEGIN
  WriteLn;
  WriteLn('Bitte die ben”tigten Daten eingeben:');
  Write('Geograph. L„nge: '); ReadLn(Laenge);
  Write('Jahr:            '); ReadLn(Jahr);
  Write('Monat:           '); ReadLn(Monat);
  Write('Tag:             '); ReadLn(Tag);
  Write('Stunden:         '); ReadLn(h);
  Write('Minuten:         '); ReadLn(m);
  Write('Sekunden:        '); ReadLn(s);
  WriteLn;
  MEZ:=Zeit(h,m,s,0);
  MOZ:=MEZtoMOZ(MEZ,Laenge);
  MOZtoSRT(MOZ,Tag,Monat,Jahr,JD,SRT);
  UhrZeit(SRT,h,m,s);
  WriteLn('Auswertung:');
  WriteLn('Zonenzeit (Dezimalwert): ',MEZ:6:4);
  WriteLn('Mittl. Ortszeit:         ',MOZ:6:4);
  WriteLn('Julianische Zeit:        ',JD:1:1);
  WriteLn('Sternzeit:               ',SRT:6:4,' (=',h:2,':',m:2,':',s:2,')');
  ReadLn
END.