§rem -*-java-*-
§rem ; Hi-lock: (("^§[a-z]*" (0 (quote hi-yellow) t)))
§rem duplicates questions from basic.java -------------------------------------
§ask className Name of class
§select clAcc u public o protected i private 0 ""
§if hasBaseClass
§ask baseClass Base class name
§fi hasBaseClass
§if isInterface Is this an interface
§defbool notInterf nil
§else
§defbool notInterf t
§fi
§rem - Field definitions ------------------------------------------------------
§doF hasFields No. of significant fields
§ask hfName§F§ Field name
§select hfAccess§F§ u public o protected i private 0 ""
§defbool hfSimple§F§ t
§defbool hfWrapper§F§ nil
§define hfInitVal1_§F§
§if hfArr§F§ Is this an array
§define hfIdx§F§ [i]
§define hfIdxD§F§ []
§define hfXindent§F§ §_§§_§§_§§_§
§else
§define hfIdx§F§
§define hfIdxD§F§
§define hfXindent§F§
§fi hfArr
§if hfInitVal§F§ Has initial value
§define hfInitVal1_§F§ =§_§
§ask hfInitVal2_§F§ Initial value
§else
§define hfInitVal2_§F§
§fi hfInitVal
§if hfBool§F§ Boolean
§define hfType§F§ boolean
§else
§if hfShort§F§ Byte/char/short/int
§select hfType§F§ b byte c char s short i int
§else
§if hfLong§F§ Long
§define hfType§F§ long
§else
§if hfFloat§F§ Float
§define hfType§F§ float
§else
§if hfDouble§F§ Double
§define hfType§F§ double
§else
§defbool hfSimple§F§ nil
§if hfString§F§ String
§define hfType§F§ String
§else
§if hfIsWrapper§F§ Wrapper class for simple type
§defbool hfWrapper§F§ t
§select hfType§F§ b Byte c Character s Shortint i Integer l Long d Double f Float
§else
§if hfObj§F§ Other object ref
§ask hfType§F§ What type
§else
§define hfType§F§ ???
§fi obj
§fi wrapper
§fi string
§fi double
§fi float
§fi long
§fi short
§fi bool
§doneF
§rem -- Field definitions end -------------------------------------------------
§rem -- Member variables ------------------------------------------------------
§if notInterf
§if bRunnable Implement Runnable
    /**
     *  Main application thread.
     **/
   Thread proc1;

§fi bRunnable
§fi notInterf
§if notInterf
§doF hasFields
    /**
     *  ???
     **/
    §hfAccess§F§§ §hfType§F§§§hfIdxD§F§§ §hfName§F§§§hfInitVal1_§F§§§hfInitVal2_§F§§;

§doneF
§fi notInterf
§doF hasFields
§if hfGetter§F§ Getter for field §F§
    /**
     * Get the value of §hfName§F§§.
     * @return Copy of §hfName§F§§
     * @see #set§hfName§F§§
     **/
§if isInterface
    public §hfType§F§§§hfIdxD§F§§ get§hfName§F§§();
§else
    public §hfType§F§§§hfIdxD§F§§ get§hfName§F§§() {
       return §hfName§F§§;
    } // get§hfName§F§§
§fi isInterface

§fi hfGetter
§if hfSetter§F§ Setter for field §F§
    /**
     * Set the value of §hfName§F§§.
     * @param new§hfName§F§§ New value of §hfName§F§§
     * @see #get§hfName§F§§
     **/
§if isInterface
    public void set§hfName§F§§(§hfType§F§§§hfIdxD§F§§ new§hfName§F§§);
§else
    public void set§hfName§F§§(§hfType§F§§§hfIdxD§F§§ new§hfName§F§§) {
       §hfName§F§§= new§hfName§F§§;
    } // set§hfName§F§§
§fi isInterface

§fi hfSetter
§doneF hasFields
§rem -- Member variables end --------------------------------------------------
§rem -- Object methods --------------------------------------------------------

§if notInterf
§if implEq Implement equals/hashCode
    /**
     * Return a hash code for this object -
     * overrides <code>Object.hashCode()</code>.
     *
     * @returns a hash code value for this object.
     **/
    public int hashCode(){
        long res=17;
        int  c;
// Mutable fields should not be included!!
§doF hasFields
§if hfArr§F§
        for (i=0; i< §hfName§F§§.length; i++) {
§fi hfArr
§if hfBool§F§ Boolean
        §hfXindent§F§§c= §hfName§F§§§hfIdx§F§§? 0 : 1;
§else
§if hfShort§F§ Byte/char/short/int
        §hfXindent§F§§c= (int) §hfName§F§§§hfIdx§F§§;
§else
§if hfLong§F§ Long
        §hfXindent§F§§c= (int)(§hfName§F§§§hfIdx§F§§ ^ (§hfName§F§§§hfIdx§F§§>>>32));
§else
§if hfFloat§F§ Float
        §hfXindent§F§§c= Float.floatToIntBits(§hfName§F§§§hfIdx§F§§);
§else
§if hfDouble§F§ Double
        §hfXindent§F§§c= Double.doubleToLongBits(§hfName§F§§§hfIdx§F§§);
        §hfXindent§F§§c= (int)(c ^ (c>>>32));
§else
§if hfString§F§ String
        §hfXindent§F§§c= (§hfName§F§§§hfIdx§F§§ == null)? 0 : §hfName§F§§§hfIdx§F§§.hashCode();
§else
§if hfWrapper§F§ Wrapper class for simple type
        §hfXindent§F§§c= (§hfName§F§§§hfIdx§F§§ == null)? 0 : §hfName§F§§§hfIdx§F§§.hashCode();
§else
§if hfObj§F§ Other object ref
        §hfXindent§F§§c= (§hfName§F§§§hfIdx§F§§ == null)? 0 : §hfName§F§§§hfIdx§F§§.hashCode();
§else
        §hfXindent§F§§c= ???? §hfName§F§§§hfIdx§F§§ ????;
§fi obj
§fi wrapper
§fi string
§fi double
§fi float
§fi long
§fi short
§fi bool
        §hfXindent§F§§res= (37 * res) + c;
§if hfArr§F§
        }
§fi hfArr
§doneF
        return (int) (res % Integer.MAX_VALUE);
    } // hashCode()

    /**
     * Compare this object to <code>that</code> - indicate whether some
     * other object is "equal to" this one.
     * Overrides <code>Object.equals()</code>.
     *
     * @param that Object to compare to.
     * @returns TRUE iff <code>this</code> is equal to <code>that</code>
     **/
    public boolean equals(Object that) {
        if (that==null) return false;
        if (that==this) return true;
        if (this.getClass() != that.getClass() ) return false;
        §className§ tmp= (§className§) that;
        // return ...<sammenlign alle felter>;
// Mutable fields should not be included!!
§doF hasFields
§if hfArr§F§
        for (i=0; i< §hfName§F§§.length; i++) {
§fi hfArr
§if hfSimple§F§ Is simple type
        if ( tmp.§hfName§F§§§hfIdx§F§§ != this.§hfName§F§§§hfIdx§F§§ )
           return false;
§else
§if hfWrapper§F§
        if ( tmp.§hfName§F§§§hfIdx§F§§.§hfType§F§§Value() !=
             this.§hfName§F§§§hfIdx§F§§.§hfType§F§§Value() )
// !!! NB  Case!
           return false;
§else
        if ( !(tmp.§hfName§F§§§hfIdx§F§§.equals(this.§hfName§F§§§hfIdx§F§§)) )
           return false;
§fi hfWrapper
§fi hfSimple
§if hfArr§F§ Is this an array
        }
§fi hfArr
§doneF
        return true;
    } // equals()

§fi implEq
§if bToString Implement toString
    /**
     * Returns a string representation of <code>this</code> object.
     * Overrides <code>Object.equals()</code>.
     *
     * @return a string representation of the object.
     **/
    public String toString() {
       StringBuffer sb= new StringBuffer(getClass().getName());
       sb.append("[");
§doF hasFields
       sb.append(":"+§hfName§F§§);
§doneF
       sb.append("]");
       return sb.toString();
    } // toString()

§fi bToString
§if bComparable Implement Comparable
    /**
     * Compares <code>this</code> object with <code>that</code> for order.
     * Returns a negative integer, zero, or a positive integer as this object
     * is less than, equal to, or greater than the specified object.
     *
     * @param that the Object to be compared.
     * @return  a negative integer, zero, or a positive integer as this
     *     object is less than, equal to, or greater than the specified object.
     * @throws ClassCastException if the specified object's type prevents
     *     it from being compared to this.
     **/
    public int compareTo(Object o) throws ClassCastException {
        §className§ tmp= (§className§) o;
        sammenlign!
    } // compareTo()

§fi bComparable
§if bCloneable Implement cloneable
§if biCloneable Write special clone
    /**
     * Creates and returns a copy of <code>this</code> object.
     *
     * @return  a clone of this instance.
     * @throws CloneNotSupportedException - if this instance cannot be cloned.
     **/
    public synchronized Object clone() throws CloneNotSupportedException {
        §className§ newObj= (§className§) super.clone();
        // !!!  legg til lokale verdier - ikke bruk constructors
§doF hasFields
§if hfArr§F§
        newObj.§hfName§F§§= this.§hfName§F§§.clone(); // Array clone
§else
§if hfSimple§F§ Is simple type
        newObj.§hfName§F§§= this.§hfName§F§§;
§else
        newObj.§hfName§F§§= this.§hfName§F§§.clone(); // Object clone
§fi hfSimple
§fi hArr
§doneF
        return newObj;
    } // clone()

§fi biCloneable
§fi bCloneable
§if inclFinal Include finalize
§if inclFinalSure Sure
    /**
     * Called by the garbage collector on an object when garbage collection
     * determines that there are no more references to the object.
     *
     * @throws any exception - causes the finalization of this object to
     *     be halted, but is otherwise ignored.
     **/
    protected void finalize() throws Throwable {
    // destroy this object first, then super
       super.finalize();
    }

§fi inclFinalSure
§fi inclFinal
§fi notInterf
