#ifdef DEBUG
#   include <iostream>
#   define dprint(str) std::cout << str << std::endl
#else
#define dprint(str)  
#endif

#include "wrap/rti.h"
#include "hsFederateAmb.h"

class HsFederateAmbassador;
typedef void (*setter)(HsFederateAmbassador *, HsFunPtr *);
extern setter setters[];

class HsFederateAmbassador : public rti13::FederateAmbassador {
public:
    FunPtrFn hs_releaseFunPtr;
    inline void releaseFun(HsFunPtr fn) {
        if (hs_releaseFunPtr && fn) hs_releaseFunPtr(fn);
    }
    
    HsFederateAmbassador(FunPtrFn releaseFunPtr) {
        hs_releaseFunPtr = NULL;
        for(int i = 0; setters[i]; i++) {
            setters[i](this, NULL);
        }
        
        hs_releaseFunPtr = releaseFunPtr;
    }
    virtual ~HsFederateAmbassador()
    throw (rti13::FederateInternalError) {
        for(int i = 0; setters[i]; i++) {
            setters[i](this, NULL);
        }
    }
    
    ////////////////////////////////////
    // Federation Management Services //
    ////////////////////////////////////

    ConstPtr_to_Void hsSynchronizationPointRegistrationSucceeded;
    virtual void synchronizationPointRegistrationSucceeded (
      const char *label) // supplied C4)
    throw (
      RTI::FederateInternalError) {
          if (hsSynchronizationPointRegistrationSucceeded)
              hsSynchronizationPointRegistrationSucceeded(label);
      }

    ConstPtr_to_Void hsSynchronizationPointRegistrationFailed;
    virtual void synchronizationPointRegistrationFailed (
      const char *label) // supplied C4)
    throw (
      RTI::FederateInternalError) {
          if (hsSynchronizationPointRegistrationFailed)
              hsSynchronizationPointRegistrationFailed(label);
      }

    virtual void announceSynchronizationPoint (
      const char *label, // supplied C4
      const char *tag)   // supplied C4
    throw (
      RTI::FederateInternalError) {}

    virtual void federationSynchronized (
      const char *label) // supplied C4)
    throw (
      RTI::FederateInternalError) {}

    virtual void initiateFederateSave (
      const char *label) // supplied C4
    throw (
      RTI::UnableToPerformSave,
      RTI::FederateInternalError) {}

    virtual void federationSaved ()
    throw (
      RTI::FederateInternalError) {}

    virtual void federationNotSaved ()
    throw (
      RTI::FederateInternalError) {}

    virtual void requestFederationRestoreSucceeded (
      const char *label) // supplied C4
    throw (
      RTI::FederateInternalError) {}

    virtual void requestFederationRestoreFailed (
      const char *label,
      const char *reason) // supplied C4
    throw (
      RTI::FederateInternalError) {}

    virtual void federationRestoreBegun ()
    throw (
      RTI::FederateInternalError) {}

    virtual void initiateFederateRestore (
      const char               *label,   // supplied C4
            RTI::FederateHandle handle)  // supplied C1
    throw (
      RTI::SpecifiedSaveLabelDoesNotExist,
      RTI::CouldNotRestore,
      RTI::FederateInternalError) {}

    virtual void federationRestored ()
    throw (
      RTI::FederateInternalError) {}

    virtual void federationNotRestored ()
    throw (
      RTI::FederateInternalError) {}

    /////////////////////////////////////
    // Declaration Management Services //
    /////////////////////////////////////

    virtual void startRegistrationForObjectClass (
            RTI::ObjectClassHandle   theClass)      // supplied C1
    throw (
      RTI::ObjectClassNotPublished,
      RTI::FederateInternalError) {}

    virtual void stopRegistrationForObjectClass (
            RTI::ObjectClassHandle   theClass)      // supplied C1
    throw (
      RTI::ObjectClassNotPublished,
      RTI::FederateInternalError) {}

    ULong_to_Void hsTurnInteractionsOn;
    virtual void turnInteractionsOn (
      RTI::InteractionClassHandle theHandle) // supplied C1
    throw (
      RTI::InteractionClassNotPublished,
      RTI::FederateInternalError) {
          if (hsTurnInteractionsOn)
              hsTurnInteractionsOn(theHandle);
      }

      ULong_to_Void hsTurnInteractionsOff;
    virtual void turnInteractionsOff (
      RTI::InteractionClassHandle theHandle) // supplied C1
    throw (
      RTI::InteractionClassNotPublished,
      RTI::FederateInternalError) {
          if (hsTurnInteractionsOff)
              hsTurnInteractionsOff(theHandle);
      }

    ////////////////////////////////
    // Object Management Services //
    ////////////////////////////////

    ULong_to_ULong_to_ConstPtr_to_Void hsDiscoverObjectInstance;
    virtual void discoverObjectInstance (
            RTI::ObjectHandle          theObject,      // supplied C1
            RTI::ObjectClassHandle     theObjectClass, // supplied C1
      const char*                      theObjectName)  // supplied C4  
    throw (
      RTI::CouldNotDiscover,
      RTI::ObjectClassNotKnown,
      RTI::FederateInternalError) {
          dprint("discoverObjectInstance: " << (void *) hsDiscoverObjectInstance);
          if (hsDiscoverObjectInstance)
              hsDiscoverObjectInstance(theObject, theObjectClass, theObjectName);
      }

      ULong_to_ConstPtrX3_to_ULongX2_to_Void hsReflectAttributeValues;
    virtual void reflectAttributeValues (
            RTI::ObjectHandle                 theObject,     // supplied C1
      const RTI::AttributeHandleValuePairSet& theAttributes, // supplied C4
      const RTI::FedTime&                     theTime,       // supplied C1
      const char                             *theTag,        // supplied C4
            RTI::EventRetractionHandle        theHandle)     // supplied C1
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateOwnsAttributes,
      RTI::InvalidFederationTime,
      RTI::FederateInternalError) {
          if (hsReflectAttributeValues)
              hsReflectAttributeValues(theObject, &theAttributes, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
      }

    virtual void reflectAttributeValues (
            RTI::ObjectHandle                 theObject,     // supplied C1
      const RTI::AttributeHandleValuePairSet& theAttributes, // supplied C4
      const char                             *theTag)        // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateOwnsAttributes,
      RTI::FederateInternalError) {
          if (hsReflectAttributeValues)
              hsReflectAttributeValues(theObject, &theAttributes, NULL, theTag, 0, 0);
      }

      ULong_to_ConstPtrX3_to_ULongX2_to_Void hsReceiveInteraction;
    // 4.6
    virtual void receiveInteraction (
            RTI::InteractionClassHandle       theInteraction, // supplied C1
      const RTI::ParameterHandleValuePairSet& theParameters,  // supplied C4
      const RTI::FedTime&                     theTime,        // supplied C4
      const char                             *theTag,         // supplied C4
            RTI::EventRetractionHandle        theHandle)      // supplied C1
    throw (
      RTI::InteractionClassNotKnown,
      RTI::InteractionParameterNotKnown,
      RTI::InvalidFederationTime,
      RTI::FederateInternalError) {
          if (hsReceiveInteraction)
              hsReceiveInteraction(theInteraction, &theParameters, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
      }

    virtual void receiveInteraction (
            RTI::InteractionClassHandle       theInteraction, // supplied C1
      const RTI::ParameterHandleValuePairSet& theParameters,  // supplied C4
      const char                             *theTag)         // supplied C4
    throw (
      RTI::InteractionClassNotKnown,
      RTI::InteractionParameterNotKnown,
      RTI::FederateInternalError) {
          if (hsReceiveInteraction)
              hsReceiveInteraction(theInteraction, &theParameters, NULL, theTag, 0, 0);
      }

    ULong_to_ConstPtrX2_to_ULongX2_to_Void hsRemoveObjectInstance;
    virtual void removeObjectInstance (
            RTI::ObjectHandle          theObject, // supplied C1
      const RTI::FedTime&              theTime,   // supplied C4
      const char                      *theTag,    // supplied C4
            RTI::EventRetractionHandle theHandle) // supplied C1
    throw (
      RTI::ObjectNotKnown,
      RTI::InvalidFederationTime,
      RTI::FederateInternalError) {
          if (hsRemoveObjectInstance)
              hsRemoveObjectInstance(theObject, &theTime, theTag, theHandle.theSerialNumber, theHandle.sendingFederate);
      }

    virtual void removeObjectInstance (
            RTI::ObjectHandle          theObject, // supplied C1
      const char                      *theTag)    // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::FederateInternalError) {
        if (hsRemoveObjectInstance)
              hsRemoveObjectInstance(theObject, NULL, theTag, 0, 0);
      }

    virtual void attributesInScope (
            RTI::ObjectHandle        theObject,     // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateInternalError) {}

    virtual void attributesOutOfScope (
            RTI::ObjectHandle        theObject,     // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateInternalError) {}

    ULong_to_ConstPtr_to_Void hsProvideAttributeValueUpdate;
    virtual void provideAttributeValueUpdate (
            RTI::ObjectHandle        theObject,     // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeNotOwned,
      RTI::FederateInternalError) {
          if (hsProvideAttributeValueUpdate)
              hsProvideAttributeValueUpdate(theObject, &theAttributes);
      }

    ULong_to_ConstPtr_to_Void hsTurnUpdatesOnForObjectInstance;
    virtual void turnUpdatesOnForObjectInstance (
            RTI::ObjectHandle        theObject,     // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotOwned,
      RTI::FederateInternalError) {
          if(hsTurnUpdatesOnForObjectInstance)
              hsTurnUpdatesOnForObjectInstance(theObject, &theAttributes);
      }

    virtual void turnUpdatesOffForObjectInstance (
            RTI::ObjectHandle        theObject,      // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotOwned,
      RTI::FederateInternalError) {}

    ///////////////////////////////////
    // Ownership Management Services //
    ///////////////////////////////////

    virtual void requestAttributeOwnershipAssumption (
            RTI::ObjectHandle        theObject,         // supplied C1
      const RTI::AttributeHandleSet& offeredAttributes, // supplied C4
      const char                    *theTag)            // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeAlreadyOwned,
      RTI::AttributeNotPublished,
      RTI::FederateInternalError) {}

    virtual void attributeOwnershipDivestitureNotification (
            RTI::ObjectHandle        theObject,          // supplied C1
      const RTI::AttributeHandleSet& releasedAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeNotOwned,
      RTI::AttributeDivestitureWasNotRequested,
      RTI::FederateInternalError) {}

    virtual void attributeOwnershipAcquisitionNotification (
            RTI::ObjectHandle        theObject,         // supplied C1
      const RTI::AttributeHandleSet& securedAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeAcquisitionWasNotRequested,
      RTI::AttributeAlreadyOwned,
      RTI::AttributeNotPublished,
      RTI::FederateInternalError) {}

    virtual void attributeOwnershipUnavailable (
            RTI::ObjectHandle        theObject,         // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeAlreadyOwned,
      RTI::AttributeAcquisitionWasNotRequested,
      RTI::FederateInternalError) {}

    virtual void requestAttributeOwnershipRelease (
            RTI::ObjectHandle        theObject,           // supplied C1
      const RTI::AttributeHandleSet& candidateAttributes, // supplied C4
      const char                    *theTag)              // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeNotOwned,
      RTI::FederateInternalError) {}

    virtual void confirmAttributeOwnershipAcquisitionCancellation (
            RTI::ObjectHandle        theObject,         // supplied C1
      const RTI::AttributeHandleSet& theAttributes) // supplied C4
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::AttributeAlreadyOwned,
      RTI::AttributeAcquisitionWasNotCanceled,
      RTI::FederateInternalError) {}

    virtual void informAttributeOwnership (
      RTI::ObjectHandle    theObject,    // supplied C1
      RTI::AttributeHandle theAttribute, // supplied C1
      RTI::FederateHandle  theOwner)     // supplied C1
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateInternalError) {}

    virtual void attributeIsNotOwned (
      RTI::ObjectHandle    theObject,    // supplied C1
      RTI::AttributeHandle theAttribute) // supplied C1
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateInternalError) {}

    virtual void attributeOwnedByRTI (
      RTI::ObjectHandle    theObject,    // supplied C1
      RTI::AttributeHandle theAttribute) // supplied C1
    throw (
      RTI::ObjectNotKnown,
      RTI::AttributeNotKnown,
      RTI::FederateInternalError) {}

    //////////////////////////////
    // Time Management Services //
    //////////////////////////////

    ConstPtr_to_Void hsTimeRegulationEnabled;
    virtual void timeRegulationEnabled (
     const  RTI::FedTime& theFederateTime) // supplied C4
    throw (
      RTI::InvalidFederationTime,
      RTI::EnableTimeRegulationWasNotPending,
      RTI::FederateInternalError) {
          if (hsTimeRegulationEnabled) hsTimeRegulationEnabled(&theFederateTime);
      }

    virtual void timeConstrainedEnabled (
      const RTI::FedTime& theFederateTime) // supplied C4
    throw (
      RTI::InvalidFederationTime,
      RTI::EnableTimeConstrainedWasNotPending,
      RTI::FederateInternalError) {}

    virtual void timeAdvanceGrant (
      const RTI::FedTime& theTime) // supplied C4
    throw (
      RTI::InvalidFederationTime,
      RTI::TimeAdvanceWasNotInProgress,
      RTI::FederateInternalError) {}

    virtual void requestRetraction (
      RTI::EventRetractionHandle theHandle) // supplied C1
    throw (
      RTI::EventNotKnown,
      RTI::FederateInternalError) {}
};

ccall void *wrap_new_HsFederateAmbassador(FunPtrFn releaseFunPtr, void **out_exc) {
    wrap(return new HsFederateAmbassador(releaseFunPtr))
}

ccall void *wrap_delete_HsFederateAmbassador(void *fedAmb, void **out_exc) {
    dprint("deleting HsFederateAmbassador");
    wrap(delete (HsFederateAmbassador *) fedAmb)
}

#define setFunPtr(field)    dprint("HsFederateAmbassador: setting " << #field           \
                                    << " to " << (void *)theFunPtr);                    \
                            HsFederateAmbassador *fa = (HsFederateAmbassador *) fedAmb; \
                            if (fa->field != theFunPtr) {                               \
                                HsFunPtr tmp = (HsFunPtr) fa->field;                    \
                                fa->field = theFunPtr;                                  \
                                fa->releaseFun(tmp);                                    \
                            }

ccall void hsfa_set_synchronizationPointRegistrationSucceeded(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsSynchronizationPointRegistrationSucceeded)
}

ccall void hsfa_set_synchronizationPointRegistrationFailed(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsSynchronizationPointRegistrationFailed)
}

ccall void hsfa_set_turnUpdatesOnForObjectInstance(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTurnUpdatesOnForObjectInstance)
}

ccall void hsfa_set_discoverObjectInstance(void *fedAmb, ULong_to_ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsDiscoverObjectInstance)
}

ccall void hsfa_set_timeRegulationEnabled(void *fedAmb, ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsTimeRegulationEnabled)
}

ccall void hsfa_set_turnInteractionsOn(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsTurnInteractionsOn)
}

ccall void hsfa_set_turnInteractionsOff(void *fedAmb, ULong_to_Void theFunPtr) {
    setFunPtr(hsTurnInteractionsOff)
}

ccall void hsfa_set_removeObjectInstance(void * fedAmb, ULong_to_ConstPtrX2_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsRemoveObjectInstance)
}

ccall void hsfa_set_provideAttributeValueUpdate(void *fedAmb, ULong_to_ConstPtr_to_Void theFunPtr) {
    setFunPtr(hsProvideAttributeValueUpdate)
}

ccall void hsfa_set_reflectAttributeValues(void *fedAmb, ULong_to_ConstPtrX3_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsReflectAttributeValues)
}

ccall void hsfa_set_receiveInteraction(void *fedAmb, ULong_to_ConstPtrX3_to_ULongX2_to_Void theFunPtr) {
    setFunPtr(hsReceiveInteraction)
}

// this array is scanned through when constructing and destructing HsFederateAmbassador.
// all function pointer setters should be here.
setter setters[] =
{   (setter) &hsfa_set_synchronizationPointRegistrationSucceeded,
    (setter) &hsfa_set_synchronizationPointRegistrationFailed,
    (setter) &hsfa_set_turnUpdatesOnForObjectInstance,
    (setter) &hsfa_set_discoverObjectInstance,
    (setter) &hsfa_set_timeRegulationEnabled,
    (setter) &hsfa_set_turnInteractionsOn,
    (setter) &hsfa_set_turnInteractionsOff,
    (setter) &hsfa_set_removeObjectInstance,
    (setter) &hsfa_set_provideAttributeValueUpdate,
    (setter) &hsfa_set_reflectAttributeValues,
    (setter) &hsfa_set_receiveInteraction,
    NULL
};
