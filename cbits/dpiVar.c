#include "dpi.h"

void setDoubleAt (dpiData* buffer, uint32_t pos, double value) {
  buffer[pos].isNull = 0;
  buffer[pos].value.asDouble = value;
}

void setInt64At (dpiData* buffer, uint32_t pos, int64_t value) {
  buffer[pos].isNull = 0;
  buffer[pos].value.asInt64 = value;
}

void setBooleanAt (dpiData* buffer, uint32_t pos, int value) {
  buffer[pos].isNull = 0;
  buffer[pos].value.asBoolean = value;
}

void setIsNull (dpiData* buffer, uint32_t pos) {
  buffer[pos].isNull = 1;
}

void setTimestampAt (dpiData* buffer, uint32_t pos, int16_t year, uint8_t month, uint8_t day, uint8_t hour, uint8_t minute, uint8_t second, uint32_t fsecond, int8_t tzHourOffset, int8_t tzMinuteOffset) {
  dpiData d = buffer[pos];
  dpiData_setTimestamp
    (
      &d,
      year,
      month,
      day,
      hour,
      minute,
      second,
      fsecond,
      tzHourOffset,
      tzMinuteOffset
    );
  buffer[pos] = d;
}
