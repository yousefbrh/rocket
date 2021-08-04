using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public static class Utils
{
    public static Vector3 NormalizeThisVector(Vector3 vector)
    {
        vector.x = NormalizeThisVectorParameter(vector.x);
        vector.y = NormalizeThisVectorParameter(vector.y);
        vector.z = NormalizeThisVectorParameter(vector.z);
        return vector;
    }

    public static float NormalizeThisVectorParameter(float parameter)
    {
        if (parameter > 180) parameter -= 360;
        return parameter;
    }

    public static void UpdateLayoutGroups(GameObject go)
    {
        var layouts = go.GetComponentsInChildren<RectTransform>();
        foreach (var layout in layouts)
        {
            LayoutRebuilder.ForceRebuildLayoutImmediate(layout);
        }
    }
    
    public static List<T> GetJustOneDimensionOfArray<T>(T[,] array, int rowIndex)
    {
        var list = new List<T>();
        for (int i = 0; i < array.GetLength(1); i++)
        {
            list.Add(array[rowIndex, i]);
        }

        return list;
    }
}