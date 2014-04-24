; ==========================================================================
; PROSJEKT:    TVIST2000
; FIL:         %M% %I%
; APPLIKASJON: #Application#
; DIALOG:      #DialogName#
; MAL:         vsd.tpl v1.3
;
; BESKRIVELSE:
;    #Description#
;
; KALLBARE RUTINER:
;    #ProcedureName#
;
; HISTORIKK:
; YYMMDD  Sign.  Endring
; ------  -----  -----------------------------------------------------------
; #Date#  #User#   Opprettet
;
; AVVIK FRA STANDARD:
;
; MULIGE UTVIDELSER:
;
; DIVERSE:
;
; ==========================================================================

procedure #ProcedureName# imports #Input# :
                          exports #Ouput# :
                          employs #CurSet#
; ==========================================================================
; PROSEDYRE:    #ProcedureName#
; TYPE:         #ProcedureType#
; ACTION:
; TARGET:
;
; BESKRIVELSE:
;    #Description#
;
; GRENSESNITT:
; Type          Navn                    Bruk
; -----------   ----------------------  ------------------------------------
; IMPORTS
; EXPORTS
; IMP/EXP
; RETURNS
; EMPLOYS
; UNIV I
; UNIV O
; UNIV I/O
; CURR I
; CURR O
; CURR I/O
; GLOB I
; GLOB O
; GLOB I/O
;
; EKSEMPEL:
;
; DIVERSE:
;
;
; ==========================================================================

   @PBEGIN_?@
   @UNIVERSAL@

;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;  Deklarasjoner
;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;  --------------
;  Lokale makroer
;  --------------
   @LOCAL@_<MAKRO> = <definisjon>

;  -----------
;  Universelle
;  -----------
   universal <ENTITET|ROLLE>

;  ----------------
;  Globale variable
;  ----------------
   declare ZQ_<VAR> <decl> global
   declare XQ_<VAR> <decl> global

;  --------------
;  Automatmerking
;  --------------
   automark <ENTITET|ROLLE>

;  ----------
;  Referanser
;  ----------
   declare <REF> as reference to <ENTITET>

;  ----------
;  Funksjoner
;  ----------
   declare <FUN> as function

;  ----------
;  Parametere
;  ----------
   declare Z_<PARAM> {as <DATAELEM>|@MAKRO@}

;  ----------
;  Konstanter
;  ----------
   declare X_<KONST> {as <DATAELEM>|@MAKRO@}

;  --------
;  Variable
;  --------
   declare Z_<VAR> {as <DATAELEM>|@MAKRO@}


;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;  Init
;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;  --------------
;  Versjonsstreng
;  --------------
   <1. prosedyre i hver fil>
   W_SCCS_ID = '@@(#)TVIST %M% %I% %E%>'

;  ----------
;  Konstanter
;  ----------
   X_<KONST> = <VERDI - Skal ikke endres>

;  --------
;  Variable
;  --------
   Z_<VAR> = <VERDI - husk at lokale variable ikke har noen default verdi>

;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;  Kode
;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;  Avslutning
;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   terminate-label @LABEL_EXIT@
   @PEND_?@

end procedure  ; #ProcedureName#
