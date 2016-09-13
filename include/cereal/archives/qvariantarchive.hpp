#ifndef QVARIANTARCHIVE_H
#define QVARIANTARCHIVE_H

#include <cereal/cereal.hpp>
#include <cereal/details/util.hpp>
#include <cereal/external/base64.hpp>

#include <stack>
#include <sstream>
#include <string>
#include <cstring>

#include <QVariant>

namespace cereal
{
  class QVariantOutputArchive : public OutputArchive<QVariantOutputArchive>, public traits::TextArchive
  {

    public:
      //! Construct, outputting to the provided stream
      /*! @param stream The stream to output to.
          @param options The JSON specific options to use.  See the Options struct
                         for the values of default parameters */
      explicit QVariantOutputArchive(QVariant & value) :
        OutputArchive<QVariantOutputArchive>(this),
        value(value)
      {
      }

        //! Saves some binary data, encoded as a base64 string, with an optional name
      /*! This will create a new node, optionally named, and insert a value that consists of
          the data encoded as a base64 string */
      void saveBinaryValue( const void * data, size_t size, const char * name = nullptr )
      {
        setNextName( name );
        writeName();

        auto base64string = base64::encode( reinterpret_cast<const unsigned char *>( data ), size );
        saveValue( base64string );
      };

      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the QVariantOutputArchive */
      //! @{

      //! Starts a new node in the JSON output
      /*! The node can optionally be given a name by calling setNextName prior
          to creating the node
          Nodes only need to be started for types that are themselves objects or arrays */
      void startNode()
      {
        writeName();
        *itsNodeStack.top().first = QVariantMap();
      }

      //! Designates the most recently added node as finished
      void finishNode()
      {
        itsNodeStack.pop();
      }

      //! Sets the name for the next node created with startNode
      void setNextName( const char * name )
      {
        itsNextName = name;
      }

      //! Saves a bool to the current node
      void saveValue(bool b)                { *itsNodeStack.top().first = b; itsNodeStack.pop();                                }
      //! Saves an int to the current node
      void saveValue(int i)                 { *itsNodeStack.top().first = i; itsNodeStack.pop();                                }
      //! Saves a uint to the current node
      void saveValue(unsigned u)            { *itsNodeStack.top().first = u; itsNodeStack.pop();                                }
      //! Saves an int64 to the current node
      void saveValue(int64_t i64)           { *itsNodeStack.top().first = static_cast<qint64>(i64); itsNodeStack.pop();         }
      //! Saves a uint64 to the current node
      void saveValue(uint64_t u64)          { *itsNodeStack.top().first = static_cast<quint64>(u64); itsNodeStack.pop();        }
      //! Saves a double to the current node
      void saveValue(double d)              { *itsNodeStack.top().first = d; itsNodeStack.pop();                                }
      //! Saves a string to the current node
      void saveValue(std::string const & s) { *itsNodeStack.top().first = QString::fromStdString(s); itsNodeStack.pop();        }
      //! Saves a const char * to the current node
      void saveValue(char const * s)        { *itsNodeStack.top().first = QString::fromUtf8(s); itsNodeStack.pop();             }
      //! Saves a nullptr to the current node
      void saveValue(std::nullptr_t)        { itsNodeStack.pop();                                                        }

    private:
      // Some compilers/OS have difficulty disambiguating the above for various flavors of longs, so we provide
      // special overloads to handle these cases.

      //! 32 bit signed long saving to current node
      template <class T, traits::EnableIf<sizeof(T) == sizeof(std::int32_t),
                                          std::is_signed<T>::value> = traits::sfinae> inline
      void saveLong(T l){ saveValue( static_cast<std::int32_t>( l ) ); }

      //! non 32 bit signed long saving to current node
      template <class T, traits::EnableIf<sizeof(T) != sizeof(std::int32_t),
                                          std::is_signed<T>::value> = traits::sfinae> inline
      void saveLong(T l){ saveValue( static_cast<std::int64_t>( l ) ); }

      //! 32 bit unsigned long saving to current node
      template <class T, traits::EnableIf<sizeof(T) == sizeof(std::int32_t),
                                          std::is_unsigned<T>::value> = traits::sfinae> inline
      void saveLong(T lu){ saveValue( static_cast<std::uint32_t>( lu ) ); }

      //! non 32 bit unsigned long saving to current node
      template <class T, traits::EnableIf<sizeof(T) != sizeof(std::int32_t),
                                          std::is_unsigned<T>::value> = traits::sfinae> inline
      void saveLong(T lu){ saveValue( static_cast<std::uint64_t>( lu ) ); }

    public:
#ifdef _MSC_VER
      //! MSVC only long overload to current node
      void saveValue( unsigned long lu ){ saveLong( lu ); };
#else // _MSC_VER
      //! Serialize a long if it would not be caught otherwise
      template <class T, traits::EnableIf<std::is_same<T, long>::value,
                                          !std::is_same<T, std::int32_t>::value,
                                          !std::is_same<T, std::int64_t>::value> = traits::sfinae> inline
      void saveValue( T t ){ saveLong( t ); }

      //! Serialize an unsigned long if it would not be caught otherwise
      template <class T, traits::EnableIf<std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::uint32_t>::value,
                                          !std::is_same<T, std::uint64_t>::value> = traits::sfinae> inline
      void saveValue( T t ){ saveLong( t ); }
#endif // _MSC_VER

      //! Save exotic arithmetic as strings to current node
      /*! Handles long long (if distinct from other types), unsigned long (if distinct), and long double */
      template <class T, traits::EnableIf<std::is_arithmetic<T>::value,
                                          !std::is_same<T, long>::value,
                                          !std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::int64_t>::value,
                                          !std::is_same<T, std::uint64_t>::value,
                                          (sizeof(T) >= sizeof(long double) || sizeof(T) >= sizeof(long long))> = traits::sfinae> inline
      void saveValue(T const & t)
      {
        std::stringstream ss; ss.precision( std::numeric_limits<long double>::max_digits10 );
        ss << t;
        saveValue( ss.str() );
      }

      //! Write the name of the upcoming node and prepare object/array state
      /*! Since writeName is called for every value that is output, regardless of
          whether it has a name or not, it is the place where we will do a deferred
          check of our node state and decide whether we are in an array or an object.
          The general workflow of saving to the JSON archive is:
            1. (optional) Set the name for the next node to be created, usually done by an NVP
            2. Start the node
            3. (if there is data to save) Write the name of the node (this function)
            4. (if there is data to save) Save the data (with saveValue)
            5. Finish the node
          */
      void writeName()
      {
        if (itsNodeStack.empty()) {
          value = QVariantMap();
          itsNodeStack.push({&value,0});
        }

        if (itsNodeStack.top().first->type() == QVariant::Map) {
          QVariantMap& map = *reinterpret_cast<QVariantMap*>(itsNodeStack.top().first->data());

          QString name;

          if(itsNextName == nullptr)
          {
            name = QString("value%1").arg( itsNodeStack.top().second++ );
          }
          else
          {
            name = QString::fromLatin1(itsNextName);
            itsNextName = nullptr;
          }

          auto it = map.insert(name, QVariant());
          itsNodeStack.push({&*it,0});
        } else if (itsNodeStack.top().first->type() == QVariant::List) {
          QVariantList& list = *reinterpret_cast<QVariantList*>(itsNodeStack.top().first->data());
          list.append(QVariant());
          itsNodeStack.push({&(list.last()),0});
        }
      }

      //! Designates that the current node should be output as an array, not an object
      void makeArray()
      {
        *itsNodeStack.top().first = QVariantList();
      }

      //! @}

    private:
      char const * itsNextName = nullptr;            //!< The next name
      std::stack<std::pair<QVariant*,uint32_t>> itsNodeStack;
      QVariant& value;
  }; // QVariantOutputArchive

  // ######################################################################
  //! An input archive designed to load data from JSON
  /*! This archive uses RapidJSON to read in a JSON archive.
      As with the output JSON archive, the preferred way to use this archive is in
      an RAII fashion, ensuring its destruction after all data has been read.
      Input JSON should have been produced by the QVariantOutputArchive.  Data can
      only be added to dynamically sized containers (marked by JSON arrays) -
      the input archive will determine their size by looking at the number of child nodes.
      Only JSON originating from a QVariantOutputArchive is officially supported, but data
      from other sources may work if properly formatted.
      The QVariantInputArchive does not require that nodes are loaded in the same
      order they were saved by QVariantOutputArchive.  Using name value pairs (NVPs),
      it is possible to load in an out of order fashion or otherwise skip/select
      specific nodes to load.
      The default behavior of the input archive is to read sequentially starting
      with the first node and exploring its children.  When a given NVP does
      not match the read in name for a node, the archive will search for that
      node at the current level and load it if it exists.  After loading an out of
      order node, the archive will then proceed back to loading sequentially from
      its new position.
      Consider this simple example where loading of some data is skipped:
      @code{cpp}
      // imagine the input file has someData(1-9) saved in order at the top level node
      ar( someData1, someData2, someData3 );        // XML loads in the order it sees in the file
      ar( cereal::make_nvp( "hello", someData6 ) ); // NVP given does not
                                                    // match expected NVP name, so we search
                                                    // for the given NVP and load that value
      ar( someData7, someData8, someData9 );        // with no NVP given, loading resumes at its
                                                    // current location, proceeding sequentially
      @endcode
      \ingroup Archives */
  class QVariantInputArchive : public InputArchive<QVariantInputArchive>, public traits::TextArchive
  {

  private:
    typedef QVariantMap::const_iterator MemberIterator;
    typedef QVariantList::const_iterator ValueIterator;
    typedef QVariant GenericValue;

    public:
      /*! @name Common Functionality
          Common use cases for directly interacting with an QVariantInputArchive */
      //! @{

      //! Construct, reading from the provided stream
      /*! @param stream The stream to read from */
      QVariantInputArchive(const QVariant& value) :
        InputArchive<QVariantInputArchive>(this),
        itsNextName( nullptr )
      {
        if (value.type() == QVariant::List) {
          const QVariantList& list = *reinterpret_cast<const QVariantList*>(value.data());
          itsIteratorStack.emplace_back(list.constBegin(), list.constEnd(), list.size());
        } else {
          const QVariantMap& map = *reinterpret_cast<const QVariantMap*>(value.data());
          itsIteratorStack.emplace_back(map.constBegin(), map.constEnd(), map.size());
        }
      }

      ~QVariantInputArchive() = default;

      //! Loads some binary data, encoded as a base64 string
      /*! This will automatically start and finish a node to load the data, and can be called directly by
          users.
          Note that this follows the same ordering rules specified in the class description in regards
          to loading in/out of order */
      void loadBinaryValue( void * data, size_t size, const char * name = nullptr )
      {
        itsNextName = name;

        std::string encoded;
        loadValue( encoded );
        auto decoded = base64::decode( encoded );

        if( size != decoded.size() )
          throw Exception("Decoded binary data size does not match specified size");

        std::memcpy( data, decoded.data(), decoded.size() );
        itsNextName = nullptr;
      }

    private:
      //! @}
      /*! @name Internal Functionality
          Functionality designed for use by those requiring control over the inner mechanisms of
          the QVariantInputArchive */
      //! @{

      //! An internal iterator that handles both array and object types
      /*! This class is a variant and holds both types of iterators that
          rapidJSON supports - one for arrays and one for objects. */
      class Iterator
      {
        public:
          Iterator() : itsType(Null_) {}

          Iterator(MemberIterator begin, MemberIterator end, size_t size) :
            itsMemberItBegin(begin), itsMemberItEnd(end), itsMemberItCurrent(begin), itsSize(size), itsType(Member)
          { }

          Iterator(ValueIterator begin, ValueIterator end, size_t size) :
            itsValueItBegin(begin), itsValueItEnd(end), itsValueItCurrent(begin), itsSize(size), itsType(Value)
          { }

          //! Advance to the next node
          Iterator & operator++()
          {
            if (itsType == Member)
              itsMemberItCurrent++;
            else if (itsType == Value)
              itsValueItCurrent++;
            return *this;
          }

          //! Get the value of the current node
          GenericValue const & value()
          {
            switch(itsType)
            {
              case Value : return *itsValueItCurrent;
              case Member: return itsMemberItCurrent.value();
              default: throw cereal::Exception("Invalid Iterator Type!");
            }
          }

          //! Get the name of the current node, or nullptr if it has no name
          QString name() const
          {
            if( itsType == Member && (itsMemberItCurrent) != itsMemberItEnd )
              return itsMemberItCurrent.key();
            else
              return QString();
          }

          //! Adjust our position such that we are at the node with the given name
          /*! @throws Exception if no such named node exists */
          inline void search( const QString& searchName )
          {
            for( auto it = itsMemberItBegin; it != itsMemberItEnd; it++ )
            {
              const auto currentName = it.key();
              if ( currentName == searchName ) {
                itsMemberItCurrent = it;
                return;
              }
            }

            throw Exception("JSON Parsing failed - provided NVP (" + searchName.toStdString() + ") not found");
          }

          inline size_t size() const
          {
            return itsSize;
          }

        private:
          MemberIterator itsMemberItBegin, itsMemberItEnd, itsMemberItCurrent; //!< The member iterator (object)
          ValueIterator itsValueItBegin, itsValueItEnd, itsValueItCurrent;    //!< The value iterator (array)
          enum Type {Value, Member, Null_} itsType;    //!< Whether this holds values (array) or members (objects) or nothing
          size_t itsSize;
      };

      //! Searches for the expectedName node if it doesn't match the actualName
      /*! This needs to be called before every load or node start occurs.  This function will
          check to see if an NVP has been provided (with setNextName) and if so, see if that name matches the actual
          next name given.  If the names do not match, it will search in the current level of the JSON for that name.
          If the name is not found, an exception will be thrown.
          Resets the NVP name after called.
          @throws Exception if an expectedName is given and not found */
      inline void search()
      {
        // The name an NVP provided with setNextName()
        if( itsNextName )
        {
          // The actual name of the current node
          auto const actualName = itsIteratorStack.back().name();

          // Do a search if we don't see a name coming up, or if the names don't match
          if( actualName.isEmpty() || actualName != itsNextName )
            itsIteratorStack.back().search( itsNextName );
        }

        itsNextName = nullptr;
      }

    public:
      //! Starts a new node, going into its proper iterator
      /*! This places an iterator for the next node to be parsed onto the iterator stack.  If the next
          node is an array, this will be a value iterator, otherwise it will be a member iterator.
          By default our strategy is to start with the document root node and then recursively iterate through
          all children in the order they show up in the document.
          We don't need to know NVPs to do this; we'll just blindly load in the order things appear in.
          If we were given an NVP, we will search for it if it does not match our the name of the next node
          that would normally be loaded.  This functionality is provided by search(). */
      void startNode()
      {
        search();

        if (itsIteratorStack.back().value().type() == QVariant::List) {
          const QVariantList& list = *reinterpret_cast<const QVariantList*>(itsIteratorStack.back().value().data());
          itsIteratorStack.emplace_back(list.constBegin(), list.constEnd(), list.size());
        } else {
          const QVariantMap& map = *reinterpret_cast<const QVariantMap*>(itsIteratorStack.back().value().data());
          itsIteratorStack.emplace_back(map.constBegin(), map.constEnd(), map.size());
        }
      }

      //! Finishes the most recently started node
      void finishNode()
      {
        itsIteratorStack.pop_back();
        ++itsIteratorStack.back();
      }

      //! Retrieves the current node name
      /*! @return nullptr if no name exists */
      QString getNodeName() const
      {
        return itsIteratorStack.back().name();
      }

      //! Sets the name for the next node created with startNode
      void setNextName( const char * name )
      {
        itsNextName = name;
      }

      //! Loads a value from the current node - small signed overload
      template <class T, traits::EnableIf<std::is_signed<T>::value,
                                          sizeof(T) < sizeof(int64_t)> = traits::sfinae> inline
      void loadValue(T & val)
      {
        search();

        val = static_cast<T>( itsIteratorStack.back().value().toInt() );
        ++itsIteratorStack.back();
      }

      //! Loads a value from the current node - small unsigned overload
      template <class T, traits::EnableIf<std::is_unsigned<T>::value,
                                          sizeof(T) < sizeof(uint64_t),
                                          !std::is_same<bool, T>::value> = traits::sfinae> inline
      void loadValue(T & val)
      {
        search();

        val = static_cast<T>( itsIteratorStack.back().value().toUInt() );
        ++itsIteratorStack.back();
      }

      //! Loads a value from the current node - bool overload
      void loadValue(bool & val)        { search(); val = itsIteratorStack.back().value().toBool(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - int64 overload
      void loadValue(int64_t & val)     { search(); val = itsIteratorStack.back().value().value<qint64>(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - uint64 overload
      void loadValue(uint64_t & val)    { search(); val = itsIteratorStack.back().value().value<quint64>(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - float overload
      void loadValue(float & val)       { search(); val = static_cast<float>(itsIteratorStack.back().value().toReal()); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - double overload
      void loadValue(double & val)      { search(); val = itsIteratorStack.back().value().toReal(); ++itsIteratorStack.back(); }
      //! Loads a value from the current node - string overload
      void loadValue(std::string & val) { search(); val = itsIteratorStack.back().value().toString().toStdString(); ++itsIteratorStack.back(); }
      //! Loads a nullptr from the current node
      void loadValue(std::nullptr_t&)   { search(); if (!itsIteratorStack.back().value().isNull())
                throw new cereal::Exception("Expected a null QVariant!"); ++itsIteratorStack.back(); }

      // Special cases to handle various flavors of long, which tend to conflict with
      // the int32_t or int64_t on various compiler/OS combinations.  MSVC doesn't need any of this.
      #ifndef _MSC_VER
    private:
      //! 32 bit signed long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::int32_t) && std::is_signed<T>::value, void>::type
      loadLong(T & l){ loadValue( reinterpret_cast<std::int32_t&>( l ) ); }

      //! non 32 bit signed long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::int64_t) && std::is_signed<T>::value, void>::type
      loadLong(T & l){ loadValue( reinterpret_cast<std::int64_t&>( l ) ); }

      //! 32 bit unsigned long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::uint32_t) && !std::is_signed<T>::value, void>::type
      loadLong(T & lu){ loadValue( reinterpret_cast<std::uint32_t&>( lu ) ); }

      //! non 32 bit unsigned long loading from current node
      template <class T> inline
      typename std::enable_if<sizeof(T) == sizeof(std::uint64_t) && !std::is_signed<T>::value, void>::type
      loadLong(T & lu){ loadValue( reinterpret_cast<std::uint64_t&>( lu ) ); }

    public:
      //! Serialize a long if it would not be caught otherwise
      template <class T> inline
      typename std::enable_if<std::is_same<T, long>::value &&
                              sizeof(T) >= sizeof(std::int64_t) &&
                              !std::is_same<T, std::int64_t>::value, void>::type
      loadValue( T & t ){ loadLong(t); }

      //! Serialize an unsigned long if it would not be caught otherwise
      template <class T> inline
      typename std::enable_if<std::is_same<T, unsigned long>::value &&
                              sizeof(T) >= sizeof(std::uint64_t) &&
                              !std::is_same<T, std::uint64_t>::value, void>::type
      loadValue( T & t ){ loadLong(t); }
      #endif // _MSC_VER

    private:
      //! Convert a string to a long long
      void stringToNumber( std::string const & str, long long & val ) { val = std::stoll( str ); }
      //! Convert a string to an unsigned long long
      void stringToNumber( std::string const & str, unsigned long long & val ) { val = std::stoull( str ); }
      //! Convert a string to a long double
      void stringToNumber( std::string const & str, long double & val ) { val = std::stold( str ); }

    public:
      //! Loads a value from the current node - long double and long long overloads
      template <class T, traits::EnableIf<std::is_arithmetic<T>::value,
                                          !std::is_same<T, long>::value,
                                          !std::is_same<T, unsigned long>::value,
                                          !std::is_same<T, std::int64_t>::value,
                                          !std::is_same<T, std::uint64_t>::value,
                                          (sizeof(T) >= sizeof(long double) || sizeof(T) >= sizeof(long long))> = traits::sfinae>
      inline void loadValue(T & val)
      {
        std::string encoded;
        loadValue( encoded );
        stringToNumber( encoded, val );
      }

      //! Loads the size for a SizeTag
      void loadSize(size_type & size)
      {
        size = itsIteratorStack.back().size();
      }

      //! @}

    private:
      const char * itsNextName;               //!< Next name set by NVP
      std::vector<Iterator> itsIteratorStack; //!< 'Stack' of rapidJSON iterators

};

  // ######################################################################
  // JSONArchive prologue and epilogue functions
  // ######################################################################

  // ######################################################################
  //! Prologue for NVPs for JSON archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> inline
  void prologue( QVariantOutputArchive &, NameValuePair<T> const & )
  { }

  //! Prologue for NVPs for JSON archives
  template <class T> inline
  void prologue( QVariantInputArchive &, NameValuePair<T> const & )
  { }

  // ######################################################################
  //! Epilogue for NVPs for JSON archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> inline
  void epilogue( QVariantOutputArchive &, NameValuePair<T> const & )
  { }

  //! Epilogue for NVPs for JSON archives
  /*! NVPs do not start or finish nodes - they just set up the names */
  template <class T> inline
  void epilogue( QVariantInputArchive &, NameValuePair<T> const & )
  { }

  // ######################################################################
  //! Prologue for SizeTags for JSON archives
  /*! SizeTags are strictly ignored for JSON, they just indicate
      that the current node should be made into an array */
  template <class T> inline
  void prologue( QVariantOutputArchive & ar, SizeTag<T> const & )
  {
    ar.makeArray();
  }

  //! Prologue for SizeTags for JSON archives
  template <class T> inline
  void prologue( QVariantInputArchive &, SizeTag<T> const & )
  { }

  // ######################################################################
  //! Epilogue for SizeTags for JSON archives
  /*! SizeTags are strictly ignored for JSON */
  template <class T> inline
  void epilogue( QVariantOutputArchive &, SizeTag<T> const & )
  { }

  //! Epilogue for SizeTags for JSON archives
  template <class T> inline
  void epilogue( QVariantInputArchive &, SizeTag<T> const & )
  { }

  // ######################################################################
  //! Prologue for all other types for JSON archives (except minimal types)
  /*! Starts a new node, named either automatically or by some NVP,
      that may be given data by the type about to be archived
      Minimal types do not start or finish nodes */
  template <class T, traits::EnableIf<!std::is_arithmetic<T>::value,
                                      !traits::has_minimal_base_class_serialization<T, traits::has_minimal_output_serialization, QVariantOutputArchive>::value,
                                      !traits::has_minimal_output_serialization<T, QVariantOutputArchive>::value> = traits::sfinae>
  inline void prologue( QVariantOutputArchive & ar, T const & )
  {
    ar.startNode();
  }

  //! Prologue for all other types for JSON archives
  template <class T, traits::EnableIf<!std::is_arithmetic<T>::value,
                                      !traits::has_minimal_base_class_serialization<T, traits::has_minimal_input_serialization, QVariantInputArchive>::value,
                                      !traits::has_minimal_input_serialization<T, QVariantInputArchive>::value> = traits::sfinae>
  inline void prologue( QVariantInputArchive & ar, T const & )
  {
    ar.startNode();
  }

  // ######################################################################
  //! Epilogue for all other types other for JSON archives (except minimal types)
  /*! Finishes the node created in the prologue
      Minimal types do not start or finish nodes */
  template <class T, traits::EnableIf<!std::is_arithmetic<T>::value,
                                      !traits::has_minimal_base_class_serialization<T, traits::has_minimal_output_serialization, QVariantOutputArchive>::value,
                                      !traits::has_minimal_output_serialization<T, QVariantOutputArchive>::value> = traits::sfinae>
  inline void epilogue( QVariantOutputArchive & ar, T const & )
  {
    ar.finishNode();
  }

  //! Epilogue for all other types other for JSON archives
  template <class T, traits::EnableIf<!std::is_arithmetic<T>::value,
                                      !traits::has_minimal_base_class_serialization<T, traits::has_minimal_input_serialization, QVariantInputArchive>::value,
                                      !traits::has_minimal_input_serialization<T, QVariantInputArchive>::value> = traits::sfinae>
  inline void epilogue( QVariantInputArchive & ar, T const & )
  {
    ar.finishNode();
  }

  // ######################################################################
  //! Prologue for arithmetic types for JSON archives
  inline
  void prologue( QVariantOutputArchive & ar, std::nullptr_t const & )
  {
    ar.writeName();
  }

  //! Prologue for arithmetic types for JSON archives
  inline
  void prologue( QVariantInputArchive &, std::nullptr_t const & )
  { }

  // ######################################################################
  //! Epilogue for arithmetic types for JSON archives
  inline
  void epilogue( QVariantOutputArchive &, std::nullptr_t const & )
  { }

  //! Epilogue for arithmetic types for JSON archives
  inline
  void epilogue( QVariantInputArchive &, std::nullptr_t const & )
  { }

  // ######################################################################
  //! Prologue for arithmetic types for JSON archives
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void prologue( QVariantOutputArchive & ar, T const & )
  {
    ar.writeName();
  }

  //! Prologue for arithmetic types for JSON archives
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void prologue( QVariantInputArchive &, T const & )
  { }

  // ######################################################################
  //! Epilogue for arithmetic types for JSON archives
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void epilogue( QVariantOutputArchive &, T const & )
  { }

  //! Epilogue for arithmetic types for JSON archives
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void epilogue( QVariantInputArchive &, T const & )
  { }

  // ######################################################################
  //! Prologue for strings for JSON archives
  template<class CharT, class Traits, class Alloc> inline
  void prologue(QVariantOutputArchive & ar, std::basic_string<CharT, Traits, Alloc> const &)
  {
    ar.writeName();
  }

  //! Prologue for strings for JSON archives
  template<class CharT, class Traits, class Alloc> inline
  void prologue(QVariantInputArchive &, std::basic_string<CharT, Traits, Alloc> const &)
  { }

  // ######################################################################
  //! Epilogue for strings for JSON archives
  template<class CharT, class Traits, class Alloc> inline
  void epilogue(QVariantOutputArchive &, std::basic_string<CharT, Traits, Alloc> const &)
  { }

  //! Epilogue for strings for JSON archives
  template<class CharT, class Traits, class Alloc> inline
  void epilogue(QVariantInputArchive &, std::basic_string<CharT, Traits, Alloc> const &)
  { }

  // ######################################################################
  // Common JSONArchive serialization functions
  // ######################################################################
  //! Serializing NVP types to JSON
  template <class T> inline
  void CEREAL_SAVE_FUNCTION_NAME( QVariantOutputArchive & ar, NameValuePair<T> const & t )
  {
    ar.setNextName( t.name );
    ar( t.value );
  }

  template <class T> inline
  void CEREAL_LOAD_FUNCTION_NAME( QVariantInputArchive & ar, NameValuePair<T> & t )
  {
    ar.setNextName( t.name );
    ar( t.value );
  }

  //! Saving for nullptr to JSON
  inline
  void CEREAL_SAVE_FUNCTION_NAME(QVariantOutputArchive & ar, std::nullptr_t const & t)
  {
    ar.saveValue( t );
  }

  //! Loading arithmetic from JSON
  inline
  void CEREAL_LOAD_FUNCTION_NAME(QVariantInputArchive & ar, std::nullptr_t & t)
  {
    ar.loadValue( t );
  }

  //! Saving for arithmetic to JSON
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void CEREAL_SAVE_FUNCTION_NAME(QVariantOutputArchive & ar, T const & t)
  {
    ar.saveValue( t );
  }

  //! Loading arithmetic from JSON
  template <class T, traits::EnableIf<std::is_arithmetic<T>::value> = traits::sfinae> inline
  void CEREAL_LOAD_FUNCTION_NAME(QVariantInputArchive & ar, T & t)
  {
    ar.loadValue( t );
  }

  //! saving string to JSON
  template<class CharT, class Traits, class Alloc> inline
  void CEREAL_SAVE_FUNCTION_NAME(QVariantOutputArchive & ar, std::basic_string<CharT, Traits, Alloc> const & str)
  {
    ar.saveValue( str );
  }

  //! loading string from JSON
  template<class CharT, class Traits, class Alloc> inline
  void CEREAL_LOAD_FUNCTION_NAME(QVariantInputArchive & ar, std::basic_string<CharT, Traits, Alloc> & str)
  {
    ar.loadValue( str );
  }

  // ######################################################################
  //! Saving SizeTags to JSON
  template <class T> inline
  void CEREAL_SAVE_FUNCTION_NAME( QVariantOutputArchive &, SizeTag<T> const & )
  {
    // nothing to do here, we don't explicitly save the size
  }

  //! Loading SizeTags from JSON
  template <class T> inline
  void CEREAL_LOAD_FUNCTION_NAME( QVariantInputArchive & ar, SizeTag<T> & st )
  {
    ar.loadSize( st.size );
  }
} // namespace cereal

// register archives for polymorphic support
CEREAL_REGISTER_ARCHIVE(cereal::QVariantInputArchive)
CEREAL_REGISTER_ARCHIVE(cereal::QVariantOutputArchive)

// tie input and output archives together
CEREAL_SETUP_ARCHIVE_TRAITS(cereal::QVariantInputArchive, cereal::QVariantOutputArchive);

namespace cereal {
template <typename T>
QVariantMap toVariantMap(const T& value) {
    QVariant var;
    { cereal::QVariantOutputArchive archive(var); archive(value); }
    return var.toMap();
}

template <typename T>
T fromVariantMap(const QVariantMap& value) {
    const QVariant var(value);
    cereal::QVariantInputArchive archive(var);
    T result;
    archive(result);
    return result;
}
}

#define REGISTER_QVARIANTMAP_CONVERTERS(T) \
QMetaType::registerConverter<T,QVariantMap>(&cereal::toVariantMap<T>); \
QMetaType::registerConverter<QVariantMap,T>(&cereal::fromVariantMap<T>);

#endif // QVARIANTARCHIVE_H
