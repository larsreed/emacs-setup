/*
 * Java support for XSL DocBook XML to Texi convertor
 *
 * $Id: DBToTexiSupport.java,v 1.1 2000/07/06 16:51:21 steve Exp $
 *
 * Copyright & License.
 *
 * Copyright (C) 1999, 2000 Mark Burton (markb@ordern.com)
 *
 * This file is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;

public class DBToTexiSupport {

    private static Hashtable _nodeNames = new Hashtable();

    private static Hashtable _unicodeMap = new Hashtable();

    static {
	_unicodeMap.put(new Character('\u00a0'), "@ "); // nbsp
	_unicodeMap.put(new Character('\u00a1'), "@exclamdown{}"); // iexcl
	_unicodeMap.put(new Character('\u00a3'), "@pounds{}"); // pound
	_unicodeMap.put(new Character('\u00a9'), "@copyright{}"); // copy
	_unicodeMap.put(new Character('\u00bf'), "@questiondown{}"); // iquest
	_unicodeMap.put(new Character('\u00c6'), "@AE{}"); // AElig
	_unicodeMap.put(new Character('\u00df'), "@ss{}"); // szlig
	_unicodeMap.put(new Character('\u00e6'), "@ae{}"); // aelig
	_unicodeMap.put(new Character('\u2013'), "--"); // ndash
	_unicodeMap.put(new Character('\u2014'), "---"); // mdash
	_unicodeMap.put(new Character('\u2022'), "@bullet{}"); // bull
	_unicodeMap.put(new Character('\u2026'), "@dots{}"); // hellip

	_unicodeMap.put(new Character('\u0131'), "@dotless{i}"); // inodot

	_unicodeMap.put(new Character('\u00a8'), "@\"{ }"); // uml
	_unicodeMap.put(new Character('\u00e4'), "@\"a"); // auml
	_unicodeMap.put(new Character('\u00c4'), "@\"A"); // Auml
	_unicodeMap.put(new Character('\u00eb'), "@\"e"); // euml
	_unicodeMap.put(new Character('\u00cb'), "@\"E"); // Euml
	_unicodeMap.put(new Character('\u00ef'), "@\"{@dotless{i}}"); // iuml
	_unicodeMap.put(new Character('\u00cf'), "@\"I"); // Iuml
	_unicodeMap.put(new Character('\u00f6'), "@\"o"); // ouml
	_unicodeMap.put(new Character('\u00d6'), "@\"O"); // Ouml
	_unicodeMap.put(new Character('\u00fc'), "@\"u"); // uuml
	_unicodeMap.put(new Character('\u00dc'), "@\"U"); // Uuml
	_unicodeMap.put(new Character('\u00ff'), "@\"y"); // yuml
	_unicodeMap.put(new Character('\u0178'), "@\"Y"); // Yuml

	_unicodeMap.put(new Character('\u00b4'), "@'{ }"); // acute
	// <!ENTITY dblac	"&#x02DD;">
	_unicodeMap.put(new Character('\u00e1'), "@'a"); // aacute
	_unicodeMap.put(new Character('\u00c1'), "@'A"); // Aacute
	_unicodeMap.put(new Character('\u00e9'), "@'e"); // eacute
	_unicodeMap.put(new Character('\u00c9'), "@'E"); // Eacute
	_unicodeMap.put(new Character('\u00ed'), "@'{@dotless{i}}"); // iacute
	_unicodeMap.put(new Character('\u00cd'), "@'I"); // Iacute
	_unicodeMap.put(new Character('\u00f3'), "@'o"); // oacute
	_unicodeMap.put(new Character('\u00d3'), "@'O"); // Oacute
	_unicodeMap.put(new Character('\u00fa'), "@'u"); // uacute
	_unicodeMap.put(new Character('\u00da'), "@'U"); // Uacute
	_unicodeMap.put(new Character('\u00fd'), "@'y"); // yacute
	_unicodeMap.put(new Character('\u00dd'), "@'Y"); // Yacute
	_unicodeMap.put(new Character('\u0107'), "@'c"); // cacute
	_unicodeMap.put(new Character('\u0106'), "@'C"); // Cacute
	_unicodeMap.put(new Character('\u01f5'), "@'g"); // gacute
	_unicodeMap.put(new Character('\u013a'), "@'l"); // lacute
	_unicodeMap.put(new Character('\u0139'), "@'L"); // Lacute
	_unicodeMap.put(new Character('\u0144'), "@'n"); // nacute
	_unicodeMap.put(new Character('\u0143'), "@'N"); // Nacute
	// <!ENTITY odblac	"&#x0151;">
	// <!ENTITY Odblac	"&#x0150;">
	_unicodeMap.put(new Character('\u0155'), "@'r"); // racute
	_unicodeMap.put(new Character('\u0154'), "@'R"); // Racute
	_unicodeMap.put(new Character('\u015b'), "@'s"); // sacute
	_unicodeMap.put(new Character('\u015a'), "@'S"); // Sacute
	// <!ENTITY udblac	"&#x0171;">
	// <!ENTITY Udblac	"&#x0170;">
	_unicodeMap.put(new Character('\u017a'), "@'z"); // zacute
	_unicodeMap.put(new Character('\u0179'), "@'Z"); // Zacute

	_unicodeMap.put(new Character('\u00b8'), "@,{ }"); // cedil
	_unicodeMap.put(new Character('\u00e7'), "@,{c}"); // ccedil
	_unicodeMap.put(new Character('\u00c7'), "@,{C}"); // Ccedil
	_unicodeMap.put(new Character('\u0122'), "@,{G}"); // Gcedil
	_unicodeMap.put(new Character('\u0137'), "@,{k}"); // kcedil
	_unicodeMap.put(new Character('\u0136'), "@,{K}"); // Kcedil
	_unicodeMap.put(new Character('\u013c'), "@,{l}"); // lcedil
	_unicodeMap.put(new Character('\u013b'), "@,{L}"); // Lcedil
	_unicodeMap.put(new Character('\u0146'), "@,{n}"); // ncedil
	_unicodeMap.put(new Character('\u0145'), "@,{N}"); // Ncedil
	_unicodeMap.put(new Character('\u0157'), "@,{r}"); // rcedil
	_unicodeMap.put(new Character('\u0156'), "@,{R}"); // Rcedil
	_unicodeMap.put(new Character('\u015f'), "@,{s}"); // scedil
	_unicodeMap.put(new Character('\u015e'), "@,{S}"); // Scedil
	_unicodeMap.put(new Character('\u0163'), "@,{t}"); // tcedil
	_unicodeMap.put(new Character('\u0162'), "@,{T}"); // Tcedil

	_unicodeMap.put(new Character('\u00af'), "@={ }"); // macr
	_unicodeMap.put(new Character('\u0101'), "@=a"); // amacr
	_unicodeMap.put(new Character('\u0100'), "@=A"); // Amacr
	_unicodeMap.put(new Character('\u0113'), "@=e"); // emacr
	_unicodeMap.put(new Character('\u0112'), "@=E"); // Emacr
	_unicodeMap.put(new Character('\u012a'), "@=I"); // Imacr
	_unicodeMap.put(new Character('\u012b'), "@={@dotless{i}}"); // imacr
	_unicodeMap.put(new Character('\u014c'), "@=O"); // Omacr
	_unicodeMap.put(new Character('\u014d'), "@=o"); // omacr
	_unicodeMap.put(new Character('\u016b'), "@=u"); // umacr
	_unicodeMap.put(new Character('\u016a'), "@=U"); // Umacr

	_unicodeMap.put(new Character('\u00e2'), "@^a"); // acirc
	_unicodeMap.put(new Character('\u00c2'), "@^A"); // Acirc
	_unicodeMap.put(new Character('\u00ea'), "@^e"); // ecirc
	_unicodeMap.put(new Character('\u00cA'), "@^E"); // Ecirc
	_unicodeMap.put(new Character('\u00ee'), "@^{@dotless{i}}"); // icirc
	_unicodeMap.put(new Character('\u00ce'), "@^I"); // Icirc
	_unicodeMap.put(new Character('\u00f4'), "@^o"); // ocirc
	_unicodeMap.put(new Character('\u00d4'), "@^O"); // Ocirc
	_unicodeMap.put(new Character('\u00db'), "@^u"); // ucirc
	_unicodeMap.put(new Character('\u00fb'), "@^U"); // Ucirc
	_unicodeMap.put(new Character('\u0109'), "@^c"); // ccirc
	_unicodeMap.put(new Character('\u0108'), "@^C"); // Ccirc
	_unicodeMap.put(new Character('\u011d'), "@^g"); // gcirc
	_unicodeMap.put(new Character('\u011c'), "@^G"); // Gcirc
	_unicodeMap.put(new Character('\u0125'), "@^h"); // hcirc
	_unicodeMap.put(new Character('\u0124'), "@^H"); // Hcirc
	_unicodeMap.put(new Character('\u0135'), "@^{@dotless{j}}"); // jcirc
	_unicodeMap.put(new Character('\u0134'), "@^J"); // Jcirc
	_unicodeMap.put(new Character('\u015d'), "@^s"); // scirc
	_unicodeMap.put(new Character('\u015c'), "@^S"); // Scirc
	_unicodeMap.put(new Character('\u0175'), "@^w"); // wcirc
	_unicodeMap.put(new Character('\u0174'), "@^W"); // Wcirc
	_unicodeMap.put(new Character('\u0177'), "@^y"); // ycirc
	_unicodeMap.put(new Character('\u0176'), "@^Y"); // Ycirc

	_unicodeMap.put(new Character('\u00e0'), "@`a"); // agrave
	_unicodeMap.put(new Character('\u00c0'), "@`A"); // Agrave
	_unicodeMap.put(new Character('\u00e8'), "@`e"); // egrave
	_unicodeMap.put(new Character('\u00c8'), "@`E"); // Egrave
	_unicodeMap.put(new Character('\u00ec'), "@`{@dotless{i}}"); // igrave
	_unicodeMap.put(new Character('\u00cc'), "@`I"); // Igrave
	_unicodeMap.put(new Character('\u00f2'), "@`o"); // ograve
	_unicodeMap.put(new Character('\u00d2'), "@`O"); // Ograve
	_unicodeMap.put(new Character('\u00f9'), "@`u"); // ugrave
	_unicodeMap.put(new Character('\u00d9'), "@`U"); // Ugrave

	_unicodeMap.put(new Character('\u00e3'), "@~a"); // atilde
	_unicodeMap.put(new Character('\u00c3'), "@~A"); // Atilde
	_unicodeMap.put(new Character('\u00f1'), "@~n"); // ntilde
	_unicodeMap.put(new Character('\u00d1'), "@~N"); // Ntilde
	_unicodeMap.put(new Character('\u00f5'), "@~o"); // otilde
	_unicodeMap.put(new Character('\u00d5'), "@~O"); // Otilde
	_unicodeMap.put(new Character('\u0129'), "@~{@dotless{i}}"); // itilde
	_unicodeMap.put(new Character('\u0128'), "@~I"); // Itilde
	_unicodeMap.put(new Character('\u0169'), "@~u"); // utilde
	_unicodeMap.put(new Character('\u0168'), "@~U"); // Utilde
    }

    public static String report(String text) {
	System.err.println(text);
	return "";
    }

    public static String trim(String text) {
	return text.trim();
    }

    public static String expandCharacters(String text) {

	for(int i = 0; i < text.length(); ++i) {

	    char c = text.charAt(i);

	    if(c >= 128 || c == '@' || c == '{' || c == '}') {

		StringBuffer result = new StringBuffer();

		result.append(text.substring(0, i));

		while(i < text.length()) {

		    c = text.charAt(i++);

		    if(c >= 128) {
			String s = (String)_unicodeMap.get(new Character(c));
			if(s != null)
			    result.append(s);
			else {
			    System.err.println("Passing through unicode character '" + c + "'");
			    result.append(c);
			}
		    }
		    else {

			if(c == '@' || c == '{' || c == '}') 
			    result.append('@');
			    
			result.append(c);
		    }
		}

		return result.toString();
	    }
	}

	return text;
    }

    public static String squeezeSpacesAndExpandCharacters(String text) {
	int len = text.length();
	char []result = new char[len];
	int j = 0;

	boolean lastWasWhite = false;
	for(int i = 0; i < len; ++i) {
	    char c = text.charAt(i);
	    if(c <= ' ') {
		if(!lastWasWhite) {
		    result[j++] = ' ';
		    lastWasWhite = true;
		}
	    }
	    else {
		result[j++] = c;
		lastWasWhite = false;
	    }
	}

	return expandCharacters(new String(result, 0, j));
    }

    public static String makeNodeName(String name) {

	// replace all ' , . : characters with _

	return name.replace('\'', '_').replace(',', '_').replace('.', '_').replace(':', '_');
    }

    public static String makeUniqueNodeName(String name, String nodeId) {

	String firstNodeId = (String)_nodeNames.get(name);

	if(firstNodeId == null)
	    _nodeNames.put(name, nodeId);
	else if(!firstNodeId.equals(nodeId))
	    return makeNodeName(nodeId);
	    
	return makeNodeName(name);
    }

    public static double lengthInInches(String len) {
	int unit;
	unit = len.indexOf("in");
	if(unit > 0)
	    return Double.valueOf(len.substring(0, unit)).doubleValue();

	unit = len.indexOf("cm");
	if(unit > 0)
	    return Double.valueOf(len.substring(0, unit)).doubleValue() / 2.54;

	unit = len.indexOf("mm");
	if(unit > 0)
	    return Double.valueOf(len.substring(0, unit)).doubleValue() / 25.4;

	unit = len.indexOf("pt");
	if(unit > 0)
	    return Double.valueOf(len.substring(0, unit)).doubleValue() / 72.0;

	unit = len.indexOf("pi");
	if(unit > 0)
	    return Double.valueOf(len.substring(0, unit)).doubleValue() * 12.0 / 72.0;
	System.err.println("Length '" + len + "' has no units, assuming inches");
	return Double.valueOf(len).doubleValue();
    }

    public static String columnFractions(double numcols, String requestedWidths) {
	String result = "";

	if(requestedWidths.length() > 0) {
	    StringTokenizer st = new StringTokenizer(requestedWidths);
	    Vector v = new Vector();
	    double totalWidth = 0;
	    while(st.hasMoreTokens()) {
		String w = st.nextToken();
		int star = w.indexOf('*');
		double requested = 1;
		if(star > 0)
		    requested = Double.valueOf(w.substring(0, star)).doubleValue();
		else if(star < 0)
		    requested = lengthInInches(w);

		v.addElement(new Double(requested));
		totalWidth += requested;
	    }

	    while(v.size() < numcols) {
		v.addElement(new Double(1));
		totalWidth += 1;
	    }

	    if(totalWidth > 0) {

		for(int i = 0; i < numcols; ++i) {
		    double requested = ((Double)v.elementAt(i)).doubleValue();
		    if(requested == totalWidth)
			return "1";

		    String n = Double.toString(requested / totalWidth).substring(1);
		    if(n.length() > 4)
			n = n.substring(0, 4);

		    result = result + " " + n;
		}
		
		return result;
	    }
	}

	if(numcols == 1.0)
	    return "1";

	String n = Double.toString(1.0 / numcols).substring(1);
	
	if(n.length() > 4)
	    n = n.substring(0, 4);
	    
	for(int i = 0; i < numcols; ++i)
	    result = result + " " + n;

	return result;
    }
}
