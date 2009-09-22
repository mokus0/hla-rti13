//File NullFederateAmbassador13.h

#ifndef NullFederateAmbassador13_h
#define NullFederateAmbassador13_h

#include <RTI13.h>

//-----------------------------------------------------------------
//                RTI Parameter Passing Memory Conventions
//
// C1  In parameter by value.
// C2  Out parameter by reference.
// C3  Function return by value.
// C4  In parameter by const reference.  Caller provides memory.
//     Caller may free memory or overwrite it upon completion of
//     the call.  Callee must copy during the call anything it
//     wishes to save beyond completion of the call.  Parameter
//     type must define const accessor methods.
// C5  Out parameter by reference.  Caller provides reference to object.
//     Callee constructs an instance on the heap (new) and returns.
//     The caller destroys the instance (delete) at its leisure.
// C6  Function return by reference.  Callee constructs an instance on
//     the heap (new) and returns a reference.  The caller destroys the
//     instance (delete) at its leisure.
//-----------------------------------------------------------------

#ifndef NULL_AMBASSADOR_EXPORT
#define NULL_AMBASSADOR_EXPORT
#endif

namespace rti13
{

class NULL_AMBASSADOR_EXPORT 
   NullFederateAmbassador : public FederateAmbassador
{
public:

NullFederateAmbassador() {}
virtual ~NullFederateAmbassador() 
throw (FederateInternalError) {}

////////////////////////////////////
// Federation Management Services //
////////////////////////////////////

virtual void synchronizationPointRegistrationSucceeded (
  const char *label) // supplied C4)
throw (
  FederateInternalError) {}

virtual void synchronizationPointRegistrationFailed (
  const char *label) // supplied C4)
throw (
  FederateInternalError) {}

virtual void announceSynchronizationPoint (
  const char *label, // supplied C4
  const char *tag)   // supplied C4
throw (
  FederateInternalError) {}

virtual void federationSynchronized (
  const char *label) // supplied C4)
throw (
  FederateInternalError) {}

virtual void initiateFederateSave (
  const char *label) // supplied C4
throw (
  UnableToPerformSave,
  FederateInternalError) {}

virtual void federationSaved ()
throw (
  FederateInternalError) {}

virtual void federationNotSaved ()
throw (
  FederateInternalError) {}

virtual void requestFederationRestoreSucceeded (
  const char *label) // supplied C4
throw (
  FederateInternalError) {}

virtual void requestFederationRestoreFailed (
  const char *label,
  const char *reason) // supplied C4
throw (
  FederateInternalError) {}

virtual void federationRestoreBegun ()
throw (
  FederateInternalError) {}

virtual void initiateFederateRestore (
  const char               *label,   // supplied C4
        FederateHandle handle)  // supplied C1
throw (
  SpecifiedSaveLabelDoesNotExist,
  CouldNotRestore,
  FederateInternalError) {}

virtual void federationRestored ()
throw (
  FederateInternalError) {}

virtual void federationNotRestored ()
throw (
  FederateInternalError) {}

/////////////////////////////////////
// Declaration Management Services //
/////////////////////////////////////

virtual void startRegistrationForObjectClass (
        ObjectClassHandle   theClass)      // supplied C1
throw (
  ObjectClassNotPublished,
  FederateInternalError) {}

virtual void stopRegistrationForObjectClass (
        ObjectClassHandle   theClass)      // supplied C1
throw (
  ObjectClassNotPublished,
  FederateInternalError) {}

virtual void turnInteractionsOn (
  InteractionClassHandle theHandle) // supplied C1
throw (
  InteractionClassNotPublished,
  FederateInternalError) {}

virtual void turnInteractionsOff (
  InteractionClassHandle theHandle) // supplied C1
throw (
  InteractionClassNotPublished,
  FederateInternalError) {}

////////////////////////////////
// Object Management Services //
////////////////////////////////

virtual void discoverObjectInstance (
        ObjectHandle          theObject,      // supplied C1
        ObjectClassHandle     theObjectClass, // supplied C1
  const char*                      theObjectName)  // supplied C4  
throw (
  CouldNotDiscover,
  ObjectClassNotKnown,
  FederateInternalError) {}

virtual void reflectAttributeValues (
        ObjectHandle                 theObject,     // supplied C1
  const AttributeHandleValuePairSet& theAttributes, // supplied C4
  const FedTime&                     theTime,       // supplied C1
  const char                             *theTag,        // supplied C4
        EventRetractionHandle        theHandle)     // supplied C1
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateOwnsAttributes,
  InvalidFederationTime,
  FederateInternalError) {}

virtual void reflectAttributeValues (
        ObjectHandle                 theObject,     // supplied C1
  const AttributeHandleValuePairSet& theAttributes, // supplied C4
  const char                             *theTag)        // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateOwnsAttributes,
  FederateInternalError) {}

// 4.6
virtual void receiveInteraction (
        InteractionClassHandle       theInteraction, // supplied C1
  const ParameterHandleValuePairSet& theParameters,  // supplied C4
  const FedTime&                     theTime,        // supplied C4
  const char                             *theTag,         // supplied C4
        EventRetractionHandle        theHandle)      // supplied C1
throw (
  InteractionClassNotKnown,
  InteractionParameterNotKnown,
  InvalidFederationTime,
  FederateInternalError) {}

virtual void receiveInteraction (
        InteractionClassHandle       theInteraction, // supplied C1
  const ParameterHandleValuePairSet& theParameters,  // supplied C4
  const char                             *theTag)         // supplied C4
throw (
  InteractionClassNotKnown,
  InteractionParameterNotKnown,
  FederateInternalError) {}

virtual void removeObjectInstance (
        ObjectHandle          theObject, // supplied C1
  const FedTime&              theTime,   // supplied C4
  const char                      *theTag,    // supplied C4
        EventRetractionHandle theHandle) // supplied C1
throw (
  ObjectNotKnown,
  InvalidFederationTime,
  FederateInternalError) {}

virtual void removeObjectInstance (
        ObjectHandle          theObject, // supplied C1
  const char                      *theTag)    // supplied C4
throw (
  ObjectNotKnown,
  FederateInternalError) {}

virtual void attributesInScope (
        ObjectHandle        theObject,     // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateInternalError) {}

virtual void attributesOutOfScope (
        ObjectHandle        theObject,     // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateInternalError) {}

virtual void provideAttributeValueUpdate (
        ObjectHandle        theObject,     // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeNotOwned,
  FederateInternalError) {}

virtual void turnUpdatesOnForObjectInstance (
        ObjectHandle        theObject,     // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotOwned,
  FederateInternalError) {}

virtual void turnUpdatesOffForObjectInstance (
        ObjectHandle        theObject,      // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotOwned,
  FederateInternalError) {}

///////////////////////////////////
// Ownership Management Services //
///////////////////////////////////

virtual void requestAttributeOwnershipAssumption (
        ObjectHandle        theObject,         // supplied C1
  const AttributeHandleSet& offeredAttributes, // supplied C4
  const char                    *theTag)            // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeAlreadyOwned,
  AttributeNotPublished,
  FederateInternalError) {}

virtual void attributeOwnershipDivestitureNotification (
        ObjectHandle        theObject,          // supplied C1
  const AttributeHandleSet& releasedAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeNotOwned,
  AttributeDivestitureWasNotRequested,
  FederateInternalError) {}

virtual void attributeOwnershipAcquisitionNotification (
        ObjectHandle        theObject,         // supplied C1
  const AttributeHandleSet& securedAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeAcquisitionWasNotRequested,
  AttributeAlreadyOwned,
  AttributeNotPublished,
  FederateInternalError) {}

virtual void attributeOwnershipUnavailable (
        ObjectHandle        theObject,         // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeAlreadyOwned,
  AttributeAcquisitionWasNotRequested,
  FederateInternalError) {}

virtual void requestAttributeOwnershipRelease (
        ObjectHandle        theObject,           // supplied C1
  const AttributeHandleSet& candidateAttributes, // supplied C4
  const char                    *theTag)              // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeNotOwned,
  FederateInternalError) {}

virtual void confirmAttributeOwnershipAcquisitionCancellation (
        ObjectHandle        theObject,         // supplied C1
  const AttributeHandleSet& theAttributes) // supplied C4
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  AttributeAlreadyOwned,
  AttributeAcquisitionWasNotCanceled,
  FederateInternalError) {}

virtual void informAttributeOwnership (
  ObjectHandle    theObject,    // supplied C1
  AttributeHandle theAttribute, // supplied C1
  FederateHandle  theOwner)     // supplied C1
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateInternalError) {}

virtual void attributeIsNotOwned (
  ObjectHandle    theObject,    // supplied C1
  AttributeHandle theAttribute) // supplied C1
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateInternalError) {}

virtual void attributeOwnedByRTI (
  ObjectHandle    theObject,    // supplied C1
  AttributeHandle theAttribute) // supplied C1
throw (
  ObjectNotKnown,
  AttributeNotKnown,
  FederateInternalError) {}

//////////////////////////////
// Time Management Services //
//////////////////////////////

virtual void timeRegulationEnabled (
 const  FedTime& theFederateTime) // supplied C4
throw (
  InvalidFederationTime,
  EnableTimeRegulationWasNotPending,
  FederateInternalError) {}

virtual void timeConstrainedEnabled (
  const FedTime& theFederateTime) // supplied C4
throw (
  InvalidFederationTime,
  EnableTimeConstrainedWasNotPending,
  FederateInternalError) {}

virtual void timeAdvanceGrant (
  const FedTime& theTime) // supplied C4
throw (
  InvalidFederationTime,
  TimeAdvanceWasNotInProgress,
  FederateInternalError) {}

virtual void requestRetraction (
  EventRetractionHandle theHandle) // supplied C1
throw (
  EventNotKnown,
  FederateInternalError) {}
};

} // End of namespace rti13

#endif // NullFederateAmbassador13_h
