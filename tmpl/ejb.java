§rem -*-java-*-
§rem ; Hi-lock: (("^§[a-z]*" (0 (quote hi-yellow) t)))
§rem /* EJB source file template - LRE, 20011025 */
§rem ------------------------------------- Name
§default Bean (eval (tplsub-buffer-name))
§default Package (eval (tplsub-buffer-name))
§ask Package
§ask Bean
§rem ------------------------------------- Type
§if isMDB Is this an MDB
§rem MDB
§defbool isENT nil
§defbool isCMP nil
§defbool isBMP nil
§defbool isSLSB nil
§defbool isSFSB nil
§define beanIntf MessageDrivenBean, MessageListener
§define bcontext MessageDriven
§define modif public
§define pkType void
§define numCreates 0
§else
§if isSLSB Is this a SLSB
§rem SLSB
§define beanIntf SessionBean
§define bcontext Session
§define modif public
§define pkType void
§defbool isENT nil
§defbool isCMP nil
§defbool isBMP nil
§defbool isSFSB nil
§else
§if isSFSB Is this a SFSB
§define beanIntf SessionBean
§define bcontext Session
§define modif public
§define pkType void
§defbool isENT nil
§defbool isCMP nil
§defbool isBMP nil
§else
§define beanIntf EntityBean
§define bcontext Entity
§defbool isENT t
§if isCMP Is this a CMP entity bean
§rem entCMP
§defbool isBMP nil
§define modif abstract public
§else
§rem entBMP
§defbool isBMP t
§define modif public
§fi isCMP
§fi isSFSB
§fi isSLSB
§fi isMDB
§if isMDB
§else
// FILE: §Bean§.java <<<<<< Remote interface <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
package §Package§;
import java.rmi.*;
import javax.ejb.*;

public interface §Bean§ extends EJBObject {
§do numPubMeth Number of public remote methods
§ask mnpType§R§ Type for method #§R§
§ask mnpName§R§ Name of method #§R§
§ask mnpArgs§R§ Arglist for method #§R§
   public §mnpType§R§§ §mnpName§R§§(§mnpArgs§R§§) throws RemoteException;
§done numPubMeth
§if isENT
§do numFields Number of fields for setters/getters
§ask eFieldName§R§ Field name §R§ (captialized)
§ask eFieldType§R§ Field type §R§
§if setField§R§ Setter for field §R§
   public void set§eFieldName§R§§(§eFieldType§R§§ nval) throws RemoteException;
§fi setField§R§
§if getField§R§ Getter for field §R§
   public §eFieldType§R§§ get§eFieldName§R§§() throws RemoteException;
§fi setField§R§
§done numFields
§fi isENT
}
// FILE: §Bean§Home.java <<<<<< Home interface <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
package §Package§;
import java.rmi.*;
import javax.ejb.*;
§if isENT
import java.util.*;
§fi isENT

public interface §Bean§Home extends EJBHome {
§if isSLSB Is this a SLSB
   public §Bean§ create() throws CreateException, RemoteException;
§else
§if isENT
§ask creArgs Create arglist
   public §Bean§ create(§creArgs§) throws CreateException, RemoteException;
§else
   public §Bean§ create() throws CreateException, RemoteException;
§fi isENT
§do numCreates Number of additional create methods
§ask creName§R§ Name of create method #§R§
§ask creArgs§R§ Argument list of create method #§R§
   public §Bean§ create§creName§R§§(§creArgs§R§§) throws CreateException, RemoteException;
§done numCreates
§fi isSLSB
§if isENT
§ask pkType Type of primary key
   public §Bean§ findByPrimaryKey(§pkType§ id) throws FinderException, RemoteException;
§do numFinds Number of additional finder methods
§ask findName§R§ Name of find method #§R§
§ask findArgs§R§ Argument list of find method #§R§
§if find1i§R§ Returns ONE instance only
   public §Bean§ find§findName§R§§(§findArgs§R§§) throws CreateException, RemoteException;
§else
   public Collection find§findName§R§§(§findArgs§R§§) throws CreateException, RemoteException;
§fi find1i§R§
§done numCreates
§fi isENT
}
§fi isMDB
// FILE: §Bean§Bean.java <<<<<< Implementation <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
package §Package§;

import javax.ejb.*;
import java.util.*;
§if isMDB
import java.jms.*;
§else
§if isENT
import java.rmi.*;
import javax.naming.*;
§if isBMP
import javax.sql.*;
import java.sql.*;
§fi isBMP
§fi isENT
§fi isMDB

>>> §modif§ class §Bean§Bean implements §beanIntf§ {

   private §bcontext§Context ctx;
§if isBMP
§do numFields
   private §eFieldType§R§§ §eFieldName§R§§; // Liten forbokstav!!!!!
§done numFields
§fi

§if useWLS Use WLS
   private weblogic.logging.NonCatalogLogger logger =
       new weblogic.logging.NonCatalogLogger("§Bean§");
§fi

   public §Bean§Bean()            { }

   public void set§bcontext§Context(§bcontext§Context c) {
      log("set§bcontext§Context() called.");
      ctx= c;
   }
§if isENT

   public void unset§bcontext§Context(§bcontext§Context c) {
      log("unset§bcontext§Context() called.");
      ctx= null;
   }
§if isBMP

   // Support methods
   private Connection getConnection() throws Exception {
      log("getConnection called.");
      InitialContext ctx= new InitialContext();
      DataSource     ds=  (javax.sql.DataSource)
                          ctx.lookup("java:comp/env/jdbc/§Bean§DB");
      return ds.getConnection();
   }

   // This is just an example !!!!!
   private Integer getNextID() throws Exception {
      String     sql=  "select 1+max(ID) as newID from §Bean§";
      Connection conn= getConnection();
      Statement  s=    conn.createStatement();
      ResultSet  rs=   s.executeQuery(sql);
      Integer    id;

      if (!rs.next()) id=1;
      else id= rs.getInt("newID");

      rs.close();
      conn.close();
      return id;
   }
§fi isBMP
§fi isENT

   public §pkType§ ejbCreate() {
      log ("ejbCreate() called.");
§if isBMP
      // This is just an example!!!
      String            sql=  null;
      Connection        conn= null;
      PreparedStatement ps=   null;
      try {
         sql=  "insert into §Bean§ (id,txt) values(?,?)";
         conn= getConnection();
         ps=   conn.prepareStatement(sql);
         setID(getNextID());  // Kanskje ikke verdens beste metode…
         ps.setInt(1,getID());
         ps.setString(2, txt);
         if ( 1 != ps.execute() )
            throw new CreateException("No records inserted");
      }
      catch (exception e) {
         throw new CreateException("ERROR: "+e);
      }
      finally {
         ps.close();
         conn.close();
      }
§else
§if isCMP
      set(...);
      set(...);
      set(...);
      return null;  // Note!
§fi isCMP
§fi isBMP
   }
§if isBMP

   public void ejbRemove() {
      log ("ejbRemove() called.");
      // This is just an example!!!
      String            sql=  null;
      Connection        conn= null;
      Statement         ps=   null;
      try {
         sql=  "delete from §Bean§ where ID=" + getID();
         conn= getConnection();
         ps=   conn.createStatement();
         ps.executeUpdate(sql);
      }
      catch (exception e) {
         throw new RemoveException("ERROR: "+e);
      }
      finally {
         ps.close();
         conn.close();
      }
   }
§fi isBMP
§if isENT
§do numFields
§if setField§R§ Setter for field §R§
§if isBMP

   public void set§eFieldName§R§§(§eFieldType§R§§ nval) {
      log("set§eFieldName§R§§() called.");
      me.§eFieldName§R§§ = nval;
   }
§else
   §modif§ void set§eFieldName§R§§(§eFieldType§R§§ nval);
§fi isBMP
§fi setField
§if getField§R§
§if isBMP

   public §eFieldType§R§§ get§eFieldName§R§§() {
      log("get§eFieldName§R§§() called.");
      return me.§eFieldName§R§§;
   }
§else
   §modif§ §eFieldType§R§§ get§eFieldName§R§§();
§fi isBMP
§fi getField§R§
§done numFields
§fi isENT
§if isBMP

   public void ejbLoad() {
       log ("ejbLoad() called.");
      // This is just an example!!!
       String            sql=  null;
       Connection        conn= null;
       Statement         ps=   null;
       ResultSet         rs=   null;

       try {
          Integer ID= (Integer) ctx.getPrimaryKey();
          setID(ID);
          sql=  "select ID,Text from §Bean§ where ID=" + getID();
          conn= getConnection();
          ps=   conn.createStatement();
          rs=   ps.executeQuery(sql);
          if (!rs.next())
             throw new EJBException("NOT FOUND-ERROR loading #" + ID);
          set§Bean§(rs.getString("§Bean§Text");
       }
       catch (exception e) {
          throw new EJBException("ERROR loading #"+getID()+ ": " +e);
       }
       finally {
          rs.close();
          ps.close();
          conn.close();
       }
    }

   public void ejbStore() {
       log ("ejbStore() called.");
      // This is just an example!!!
       String            sql=  null;
       Connection        conn= null;
       PreparedStatement ps=   null;

       try {
          sql=  "update §Bean§ set Text=? where ID=?";
          conn= getConnection();
          ps=   conn.prepareStatement();
          ps.setString(1,get§Bean§());
          ps.setInt(2,getID());
          ps.execute();
       }
       catch (exception e) {
          throw new EJBException("ERROR updating #"+getID()+ ": " +e);
       }
       finally {
          ps.close();
          conn.close();
       }
   }

   public Integer ejbFindByPrimaryKey(Integer ID) {
       log ("ejbFindByPrimaryKey() called.");
      // This is just an example!!!
       String            sql=  null;
       Connection        conn= null;
       Statement         ps=   null;
       ResultSet         rs=   null;

       try {
          sql=  "select * from §Bean§ where helloID=" + ID;
          conn= getConnection();
          ps=   conn.createStatement();
          rs=   ps.executeQuery(sql);
          if (!rs.next())
             throw new ObjectNotFoundException("ERROR: could not find #" +
                                               ID);
       }
       catch (exception e) {
          throw new EJBException("ERROR finding #"+getID()+ ": " +e);
       }
       finally {
          rs.close();
          ps.close();
          conn.close();
       }
       return ID;
   }
§do numFinds

§if find1i§R§
   public §Bean§ ejbFind§findName§R§§(§findArgs§R§§) {
§else
   public Collection ejbFind§findName§R§§(§findArgs§R§§) {
      // This is just an example!!!
      String            sql=  null;
      Connection        conn= null;
      Statement         ps=   null;
      ResultSet         rs=   null;
      Vector            coll= new Vector();

      try {
         sql=  "select ID from §Bean§";
         conn= getConnection();
         ps=   conn.createStatement();
         rs=   ps.executeQuery(sql);
         while (rs.next())
            coll.addElement(rs.getInt("ID"));
      }
      catch (exception e) {
         throw new EJBException("ERROR finding #"+getID()+ ": " +e);
      }
      finally {
         rs.close();
         ps.close();
         conn.close();
      }
      return coll;
§fi find1i§R§
   }
§done numCreates
§fi isBMP
§if isSLSB
§rem One create only
§else
§do numCreates

   public void ejbCreate§creName§R§§(§creArgs§R§§) {
      log("ejbCreate§creName§R§§() called.");
   }
§done numCreates
§fi isSLSB

§if isENT
§else
   public void ejbRemove()        { log("ejbRemove() called."); }
§fi
§if isMDB
§else
   public void ejbPassivate()     { log("ejbPassivate() called."); }
   public void ejbActivate()      { log("ejbActivate() called."); }
§if isENT
   public void ejbPostCreate()    { log("ejbPostCreate() called."); }
§if isCMP
   public void ejbLoad()         { log("ejbLoad() called"); }
   public void ejbStore()        { log("ejbStore() called"); }
   public void ejbRemove()       { log("ejbRemove() called"); }
§fi isCMP
§fi isENT
§fi isMDB

   private void log (String logMessage) {
§if useWLS
       logger.debug(this.getClass().getName() + ": " + logMessage);
§else
       // implement this!
       // logger.debug(this.getClass().getName() + ": " + logMessage);
§fi
   }
§if isMDB
   public void onMessage(Message msg) {
      TextMessage tm= (TextMessage) msg;
      try {
         String s= tm.getText();
      }
      catch (JMSException je) {
         je.printStackTrace();
      }
   }
§else !isMDB
§do numPubMeth

   public §mnpType§R§§ §mnpName§R§§(§mnpArgs§R§§) {
   } // §mnpName§R§§()
§done
§fi isMDB
}
