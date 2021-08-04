using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class SplashController : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        StartCoroutine(LoadMenu());
    }

    private IEnumerator LoadMenu()
    {
        yield return new WaitForSeconds(3f);
        SceneManager.LoadScene("Menu");
    }
}
