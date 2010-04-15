{$DEFINE Kopro}
UNIT Vectors;


{
  ************************************************************************
  *****                                                              *****
  *****                    V E C T O R S . P A S                     *****
  *****               (c) 1991,92 by Wolfram Schroers                *****
  *****                                                              *****
  *****            Ein Untermodul von ASTROCAD v1.0 und ASTCONV      *****
  *****                                                              *****
  ************************************************************************

  Diese UNIT exportiert die gesamten Vektor- und Albgebrafunktionen und
  -Prozeduren, die von den meisten Teilen von ASTROCAD gebraucht werden.
  Sie muá somit in jedem Programmteil in der USES-Anweisung aufgerufen
  werden. Es werden auch einige Anwendungsspezifische Teile (z.B. TWatch)
  exportiert. Aus- oder Eingaben in eine Datei oder dergleichen finden
  nicht (!) statt.

  Version: v1.2a
}


{$IFDEF Kopro}
{$N+,E+}
{$ELSE}
{$N-,E-}
{$ENDIF}
{S-,R-}


INTERFACE


CONST MaxMatrix = 8;


{$IFDEF Kopro}
TYPE  Skalar    = Extended;
      Real      = Extended;
{$ELSE}
TYPE  Skalar    = Real;
{$ENDIF}
      TVector   = RECORD
                    X,Y,Z:Skalar                { Ein Vektor im R3 }
                  END;
      TPlane    = RECORD
                    A,B,C,D:Skalar              { Eine Ebene in Normalenform }
                  END;
      TWatch    = RECORD                        { Ein K”rper am Bildschirm }
                    Bildebene:TPlane;           { Die Bildebene (als TPlane) }
                    MittelP:TVector;            { Mittelpunkt des Bildschirmes }
                    Zoom:Skalar;                { Der Verkleinerungsfaktor }
                    Punkt:TVector;              { Der Ortsvektor des K”rpers }
                    BRad:Skalar;                { Der Radius des K”rpers }
                    PixelX,PixelY,PRad:Integer  { Die Bildschirmkoordinaten des K”rpers }
                  END;
      TMatrix   = ARRAY[1..MaxMatrix+1,1..MaxMatrix] OF Skalar;


CONST NoPers    = FALSE;                        { Keine Perspektive bei TWatch }
      AutoPers  = TRUE;                         { Perspektive bei TWatch }
      NullVector:TVector=(X:0;Y:0;Z:0);         { Der Nullvektor }
 

{
        Deklarationsteil der Prozeduren und Funktionen
}

  FUNCTION VLen(v:TVector):Skalar;
   { L„nge des Vektors v }

  PROCEDURE VecNorm(VAR v:TVector);
   { Normiert den Vektor v }

  PROCEDURE VSub(VAR C:TVector;A,B:TVector);
   { c:=a-b Vektorsubtraktion }

  PROCEDURE VAdd(VAR C:TVector;A,B:TVector);
   { c:=a+b Vektoraddition }

  PROCEDURE VSMult(VAR C:TVector;S:Skalar);
   { c:=s*c Skalarmultiplikation }

  FUNCTION VSProd(A,B:TVector):Skalar;
   { VSProd:=a*b Standardskalarprodukt von a und b }

  PROCEDURE VCProd(VAR C:TVector;A,B:TVector);
   { c:=axb Kreuzprodukt von a und b }

  PROCEDURE VRotate(VAR A:TVector;Angle:Skalar);
   { Rotatiert A um den Winkel Angle (in ø) }

  PROCEDURE VRot_x(VAR A:TVector;Angle:Skalar);
   { Rotiert A um die x-Achse (Winkel Angle in rad) }

  PROCEDURE VRot_y(VAR A:TVector;Angle:Skalar);
   { Rotiert A um die y-Achse (Winkel Angle in rad) }

  PROCEDURE VRot_z(VAR A:TVector;Angle:Skalar);
   { Rotiert A um die z-Achse (Winkel Angle in rad) }

  PROCEDURE PDef(VAR P:TPlane;X,Y,Z:TVector);
   { Definiert die Ebene P durch die drei Punkte x,y und z }

  FUNCTION PPointDist(P:TPlane;Vec:TVector):Skalar;
   { Abstand des Punktes Vec von der Ebene P (bitte im Betrag nehmen!) }

  FUNCTION PPointSide(P:TPlane;Vec:TVector):Skalar;
   { Bei PPointSide=-1 liegen der Ursprung und Vec auf derselben Seite von P }

  PROCEDURE PPointProj(VAR Proj:TVector;P:TPlane;Vec:TVector);
   { Lotfuápunkt des Punktes Vec auf die Ebene P; Proj ist dabei der LFP }

  PROCEDURE LPointProj(VAR Proj:TVector;A,X,Y:TVector);
   { Lotfuápunkt Proj von A auf die Gerade, die X und Y enth„lt }

  FUNCTION LPointDist(A,X,Y:TVector):Skalar;
   { Abstand Punkt A-Gerade, die durch X und Y geht anhand des Lotfuápunktes }

  PROCEDURE LP_Intsec(Ebene:TPlane;P1,P2:TVector;VAR HitPoint:TVector);
   { Die Gerade durch die Punkte schneidet die Ebene in dem Punkt
     "HitPoint", der zurckgeliefert wird (g(P1,P2) darf nicht parallel
     zu Ebene sein!) }

  PROCEDURE Body_to_screen(VAR Data:TWatch);
   { Ermittelt die Bildschirmkoordinaten eines Punktes; siehe TWatch }

  FUNCTION BitSet(S:Word;BitNr:Byte):Boolean;
   { Testet, ob das BitNr.-te Bit gesetzt ist oder nicht (Z„hlung:0-7) }


IMPLEMENTATION


CONST ErdRadius   = 6378137;
      MondRadius  = 3476000;
        



  FUNCTION Sgn(x:Skalar):Skalar;
  BEGIN
    IF x>0 THEN Sgn:=1 ELSE IF x<0 THEN Sgn:=-1 ELSE Sgn:=0
  END;

  FUNCTION VLen(v:TVector):Skalar;
  BEGIN
    WITH v DO VLen:=Sqrt(X*X+Y*Y+Z*Z)
  END;

  PROCEDURE VecNorm(VAR v:TVector);
  VAR Hilf:Skalar;
  BEGIN
    Hilf:=VLen(v);
    WITH v DO BEGIN x:=x/Hilf; y:=y/Hilf; z:=z/Hilf END
  END;

  PROCEDURE VSub(VAR C:TVector;A,B:TVector);
  BEGIN
    WITH C DO
    BEGIN
      X:=A.X-B.X;
      Y:=A.Y-B.Y;
      Z:=A.Z-B.Z
    END
  END;

  PROCEDURE VAdd(VAR C:TVector;A,B:TVector);
  BEGIN
    WITH C DO
    BEGIN
      X:=A.X+B.X;
      Y:=A.Y+B.Y;
      Z:=A.Z+B.Z
    END
  END;

  PROCEDURE VSMult(VAR C:TVector;S:Skalar);
  BEGIN
    WITH C DO
    BEGIN
      X:=S*X;
      Y:=S*Y;
      Z:=S*Z
    END
  END;

  FUNCTION VSProd(A,B:TVector):Skalar;
  BEGIN
    WITH A DO VSProd:=X*B.X+Y*B.Y+Z*B.Z
  END;

  PROCEDURE VCProd(VAR C:TVector;A,B:TVector);
  BEGIN
    WITH C DO
    BEGIN
      X:=A.Y*B.Z-A.Z*B.Y;
      Y:=-(A.X*B.Z-A.Z*B.X);
      Z:=A.X*B.Y-A.Y*B.X
    END
  END;

  PROCEDURE VRotate(VAR A:TVector;Angle:Skalar);
  VAR Hilf:TVector;
  BEGIN
    Angle:=Angle/180*Pi;
    WITH Hilf DO
    BEGIN
      x:=a.x*Cos(Angle)-a.y*Sin(Angle);
      y:=a.x*Sin(Angle)+a.y*Cos(Angle);
      z:=0
    END;
    A:=Hilf
  END;

  PROCEDURE VRot_x(VAR A:TVector;Angle:Skalar);
  VAR Hilf:TVector;
  BEGIN
    WITH Hilf DO
    BEGIN
      x:=A.x;
      y:=A.y*Cos(Angle)-A.z*Sin(Angle);
      z:=A.y*Sin(Angle)+A.z*Cos(Angle)
    END;
    A:=Hilf
  END;

  PROCEDURE VRot_y(VAR A:TVector;Angle:Skalar);
  VAR Hilf:TVector;
  BEGIN
    WITH Hilf DO
    BEGIN
      x:=A.x*Cos(Angle)+A.z*Sin(Angle);
      y:=A.y;
      z:=A.z*Cos(Angle)-A.x*Sin(Angle)
    END;
    A:=Hilf
  END;

  PROCEDURE VRot_z(VAR A:TVector;Angle:Skalar);
  VAR Hilf:TVector;
  BEGIN
    WITH Hilf DO
    BEGIN
      x:=A.x*Cos(Angle)-A.y*Sin(Angle);
      y:=A.x*Sin(Angle)-A.y*Cos(Angle);
      z:=A.z
    END;
    A:=Hilf
  END;

  PROCEDURE PDef(VAR P:TPlane;X,Y,Z:TVector);
  VAR K,L:TVector;
  BEGIN
    VSub(K,X,Y); VSub(L,X,Z);
    WITH P DO
    BEGIN
      A:=K.Y*L.Z-K.Z*L.Y;
      B:=-(K.X*L.Z-K.Z*L.X);
      C:=K.X*L.Y-L.X*K.Y;
      D:=-A*X.X-B*X.Y-C*X.Z
    END
  END;

  FUNCTION PPointDist(P:TPlane;Vec:TVector):Skalar;
  VAR Normale:TVector;
  BEGIN
    WITH Normale DO BEGIN X:=P.A; Y:=P.B; Z:=P.C END;
    PPointDist:=-Sgn(P.D)/VLen(Normale)*(Vec.X*P.A+Vec.Y*P.B+Vec.Z*P.C+P.D)
  END;

  FUNCTION PPointSide(P:TPlane;Vec:TVector):Skalar;
  BEGIN
    PPointSide:=Sgn(PPointDist(P,Vec))
  END;

  PROCEDURE PPointProj(VAR Proj:TVector;P:TPlane;Vec:TVector);
  VAR Eta:Real;
  BEGIN
    WITH P DO
    BEGIN
      WITH Vec DO Eta:=-((A*X+B*Y+C*Z+D)/(A*A+B*B+C*C));
      WITH Proj DO
      BEGIN
        X:=Vec.X+Eta*A;
        Y:=Vec.Y+Eta*B;
        Z:=Vec.Z+Eta*C
      END
    END
  END;

  PROCEDURE LPointProj(VAR Proj:TVector;A,X,Y:TVector);
  VAR n,C,D:TVector;
      Det,Detk,k:Skalar;
  BEGIN
    VSub(C,X,Y); { Geradenvektor der Gerade, die X und Y enth„lt }
    VSub(D,A,X);
    VCProd(n,D,C);
    VCProd(n,n,C);
    VSub(D,A,X);
    Det:=n.X*C.Y-n.Y*C.X;
    Detk:=D.X*C.Y-D.Y*C.X;
    k:=Detk/Det;
    WITH Proj DO
    BEGIN
      X:=A.X+k*n.X;
      Y:=A.Y+k*n.Y;
      Z:=A.Z+k*n.Z
    END
  END;

  PROCEDURE LP_Intsec(Ebene:TPlane;P1,P2:TVector;VAR HitPoint:TVector);
  VAR u:TVector;
      r:Skalar;
  BEGIN
    VSub(u,P1,P2);
    WITH Ebene DO
      r:=-(a*P1.x+b*P1.y+c*P1.z+d)/(a*u.x+b*u.y+c*u.z);
    VSMult(u,r);
    VAdd(HitPoint,P1,u)
  END;

  FUNCTION LPointDist(A,X,Y:TVector):Skalar;
  VAR ProjPoint:TVector;
  BEGIN
    LPointProj(ProjPoint,A,X,Y);
    VSub(ProjPoint,ProjPoint,A);
    LPointDist:=VLen(ProjPoint)
  END;

  PROCEDURE Body_to_screen(VAR Data:TWatch);
  VAR Hilf:TVector;
  BEGIN
    WITH Data DO
    BEGIN
      VSub(Punkt,Punkt,MittelP);
      PRad:=Round(BRad/Zoom);
      PPointProj(Hilf,Bildebene,Punkt);
      PixelX:=Round(Hilf.X/Zoom); PixelY:=Round(Hilf.Y/Zoom)
    END
  END;

  FUNCTION BitSet(S:Word;BitNr:Byte):Boolean;
  BEGIN
    BitSet:=((S SHR BitNr) AND 1)=1
  END;


BEGIN
END.
