#pragma once

#include <vector>
#include <deque>
#include <memory>
#include <exception>
#include <iostream>

namespace rubbish {
	namespace type {
		
		struct StringExceptionBase {
			const std::string reason;
			
			StringExceptionBase(const std::string&& reason);

			const std::string& GetReason() const;
		};

		struct InvalidTypeApplicationException 
			: public StringExceptionBase, public std::exception {
			InvalidTypeApplicationException(const std::string&& reason);
		};

		struct MismatchedTypesException
			: public StringExceptionBase, public std::exception {
			MismatchedTypesException(const std::string&& reason);
		};
		
		enum TypeOfType {
			TYPE_CONSTANT,
			TYPE_VARIABLE
		};

		struct Type {
			int typeId;
			TypeOfType typeOfType;

			std::deque<Type> params;

			Type();
			Type(int id, TypeOfType typeOfType);

			void AddParam(const Type& t);
			void Apply(const Type& t2);
			
			bool operator==(const Type& t2) const;
		};

		enum TypeOfValue {
			VALUE_CONSTANT,
			VALUE_FUNCTION
		};

		struct FunctionInfo;

		struct Value {
			Type type;
			TypeOfValue typeOfValue;
			FunctionInfo* fInfo;

		};
	}
}