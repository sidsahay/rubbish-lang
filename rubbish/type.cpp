#include "type.h"

rubbish::type::StringExceptionBase::StringExceptionBase(const std::string&& reason)
	: reason(std::move(reason)) {
}

const std::string& rubbish::type::StringExceptionBase::GetReason() const {
	return reason;
}

rubbish::type::InvalidTypeApplicationException::InvalidTypeApplicationException(const std::string&& reason) 
	: StringExceptionBase(std::move(reason)) {
}

rubbish::type::MismatchedTypesException::MismatchedTypesException(const std::string&& reason)
	: StringExceptionBase(std::move(reason)) {
}

rubbish::type::Type::Type(int id, TypeOfType typeOfType) : typeId(id), typeOfType(typeOfType) {
}

void rubbish::type::Type::AddParam(const Type& t) {
	params.push_back(t);
}

void rubbish::type::Type::Apply(const Type& t2) {
	if (params.size() < 1) {
		throw InvalidTypeApplicationException("Invalid application of type to type with no free slots.");
	}

	const auto& firstTypeParam = params.front();

	if (t2 == firstTypeParam) {
		params.pop_front();
	}
	else {
		throw MismatchedTypesException("Expected and actual types differ in type application.");
	}
}

bool rubbish::type::Type::operator==(const Type& t2) const {
	//equal if concretely equal or both are type variables
	if ((typeOfType == TYPE_CONSTANT && t2.typeOfType == TYPE_CONSTANT && typeId == t2.typeId) 
		|| (typeOfType == TYPE_VARIABLE && typeOfType == TYPE_VARIABLE)) {
		if (params.size() == t2.params.size()) {
			if (params.size() == 0) {
				return true;
			}
			else {
				bool acc = true;
				for (auto it = params.begin(), it2 = t2.params.begin();
					it != params.end() && it2 != t2.params.end();
					it++, it2++) {
					acc = acc && (*it == *it2);
				}
				return acc;
			}
		}
		else {
			return false;
		}
	}
	else {
		return false;
	}
}