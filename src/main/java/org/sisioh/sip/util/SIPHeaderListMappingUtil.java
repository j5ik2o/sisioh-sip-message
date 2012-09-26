package org.sisioh.sip.util;

import org.sisioh.sip.message.header.impl.SIPHeader;
import org.sisioh.sip.message.header.impl.SIPHeaderList;
import org.sisioh.sip.message.header.impl.SIPHeaderListMapping;
import scala.Option;
import scala.Some;
import scala.collection.immutable.List;
import scala.collection.immutable.List$;

import java.lang.reflect.InvocationTargetException;

public class SIPHeaderListMappingUtil {

    public static <T extends SIPHeaderList<T, HDR>, HDR extends SIPHeader> Option<SIPHeaderList<T, HDR>> getList(SIPHeader sipHeader) {
        try {
            Class<?> headerClass = sipHeader.getClass();
            Option<Class<?>> listClass = SIPHeaderListMapping.getListClass(headerClass);
            if (listClass.isDefined()) {
                @SuppressWarnings("unchecked")
                SIPHeaderList<T, HDR> shl = (SIPHeaderList<T, HDR>) listClass.get().getConstructor(List.class).newInstance(List$.MODULE$.empty());
                return new Some<SIPHeaderList<T, HDR>>(shl);
            } else {
                return Option.empty();
            }
        } catch (InstantiationException ex) {
            throw new RuntimeException(ex);
        } catch (IllegalAccessException ex) {
            throw new RuntimeException(ex);
        } catch (NoSuchMethodException ex) {
            throw new RuntimeException(ex);
        } catch (InvocationTargetException ex) {
            throw new RuntimeException(ex);
        }
    }

}
