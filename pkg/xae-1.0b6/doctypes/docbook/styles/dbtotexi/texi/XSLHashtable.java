/* 
 * Two-way hashtable with Strings only.
 *
 * $Id: XSLHashtable.java,v 1.1 2000/08/14 18:01:51 stevecheng Exp $
 */

import java.util.Hashtable;

public class XSLHashtable {
    private Hashtable keytable;
    private Hashtable valtable;

    public XSLHashtable()
    {
	keytable = new Hashtable();
	valtable = new Hashtable();
    }

    public String getValue(String key)
    {
	return (String)valtable.get(key);
    }

    public String getKey(String value)
    {
	return (String)keytable.get(value);
    }

    public boolean containsKey(String key)
    {
	return valtable.containsKey(key);
    }

    public boolean containsValue(String value)
    {
	return keytable.containsKey(value);
    }

    public boolean put(String key, String value)
    {
	keytable.put(value, key);
	return (valtable.put(key, value)==null);
    }
}

