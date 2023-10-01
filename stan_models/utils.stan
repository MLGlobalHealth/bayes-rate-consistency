/** Append four int type arrays */
array[] int append_array_int4(array[] int a, array[] int b, array[] int c, array[] int d)
{
  return append_array(a, append_array(b, append_array(c, d)));
}

/** Append four row vectors */
vector append_row_4(vector a, vector b, vector c, vector d)
{
  return append_row(a, append_row(b, append_row(c, d)));
}