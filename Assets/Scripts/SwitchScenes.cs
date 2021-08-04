using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class SwitchScenes : MonoBehaviour
{
    public string sceneName;                    // The name of the scene to be loaded.
    public float delay = 0f;

    private void OnTriggerEnter(Collider other)
    {
        ChangeScene();
    }

    public void ChangeScene()
    {
        SceneManager.UnloadSceneAsync(SceneManager.GetActiveScene().name);
        SceneManager.LoadScene(sceneName);
//        StartCoroutine(FadeAndSwitchScenes(sceneName));
    }
    
    private IEnumerator FadeAndSwitchScenes (string sceneName)
    {
        
        yield return SceneManager.UnloadSceneAsync (SceneManager.GetActiveScene ().buildIndex);
        yield return StartCoroutine (LoadSceneAndSetActive (sceneName));
    }
    
    private IEnumerator LoadSceneAndSetActive (string sceneName)
    {
        // Allow the given scene to load over several frames and add it to the already loaded scenes (just the Persistent scene at this point).
        yield return SceneManager.LoadSceneAsync (sceneName, LoadSceneMode.Additive);

        // Find the scene that was most recently loaded (the one at the last index of the loaded scenes).
        Scene newlyLoadedScene = SceneManager.GetSceneAt (SceneManager.sceneCount - 1);

        // Set the newly loaded scene as the active scene (this marks it as the one to be unloaded next).
        SceneManager.SetActiveScene (newlyLoadedScene);
    }
}
