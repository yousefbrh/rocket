using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class CurrencyModel : MonoBehaviour
{
    [SerializeField] private TextMeshProUGUI currencyText;
    private void Start()
    {
        CurrencyManager.instance.OnCurrencyChange += UpdateText;
        UpdateText(CurrencyManager.instance.GetCurrentValue());
    }

    private void UpdateText(int value)
    {
        currencyText.text = value.ToString();
        Utils.UpdateLayoutGroups(gameObject);
    }
    //
    // private void OnDisable()
    // {
    //     CurrencyManager.instance.OnCurrencyChange -= UpdateText;
    // }
}